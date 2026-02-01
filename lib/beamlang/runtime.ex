defmodule BeamLang.Runtime do
  @moduledoc """
  Compiles Erlang abstract forms to BEAM and executes main/0.
  """

  alias BeamLang.Codegen

  @spec compile_forms(list()) :: {:ok, atom(), binary()} | {:error, map()}
  def compile_forms(forms) do
    case :compile.forms(forms, [:return]) do
      {:ok, module, binary} -> {:ok, module, binary}
      {:ok, module, binary, _warnings} -> {:ok, module, binary}
      {:error, errors, warnings} -> {:error, %{message: "BEAM compile failed: #{inspect({errors, warnings})}"}}
    end
  end

  @spec load_and_run(atom(), binary(), list()) :: {:ok, term()} | {:error, map()}
  def load_and_run(module, binary, args \\ []) do
    case :code.load_binary(module, ~c"beamlang_program.beam", binary) do
      {:module, ^module} ->
        # Convert raw Erlang list to BeamLang List struct
        beamlang_list = wrap_as_beamlang_list(args)
        {:ok, apply(module, :main, [beamlang_list])}
      {:error, reason} -> {:error, %{message: "Failed to load compiled module: #{inspect(reason)}"}}
    end
  end

  @doc """
  Wraps a raw Erlang list into a BeamLang List struct.
  Delegates to the compiled stdlib's list_from_data function.
  """
  @spec wrap_as_beamlang_list(list()) :: map()
  def wrap_as_beamlang_list(data) when is_list(data) do
    apply(:beamlang_program, :list_from_data, [data])
  end

  @spec load_modules([{atom(), binary()}]) :: :ok | {:error, map()}
  def load_modules(modules) when is_list(modules) do
    Enum.reduce_while(modules, :ok, fn {module, binary}, _acc ->
      case :code.load_binary(module, ~c"beamlang_module.beam", binary) do
        {:module, ^module} -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, %{message: "Failed to load compiled module: #{inspect(reason)}"}}}
      end
    end)
  end

  @spec compile_and_run(BeamLang.AST.t()) :: {:ok, term()} | {:error, map()}
  def compile_and_run(ast) do
    forms = Codegen.to_erlang_forms(ast)

    with {:ok, module, binary} <- compile_forms(forms),
         {:ok, value} <- load_and_run(module, binary) do
      {:ok, value}
    end
  end

  @spec typeof_data(term()) :: charlist()
  def typeof_data(value) do
    cond do
      is_integer(value) or is_float(value) -> ~c"number"
      is_binary(value) -> ~c"String"
      is_boolean(value) -> ~c"bool"
      value == :ok -> ~c"void"
      is_map(value) and Map.has_key?(value, :__beamlang_type__) ->
        ensure_charlist(Map.get(value, :__beamlang_type__))
      is_map(value) and Map.get(value, :kind) == 1 -> ~c"Optional"
      is_map(value) and Map.get(value, :kind) == 2 -> ~c"Result"
      is_map(value) and string_map?(value) -> ~c"String"
      is_tuple(value) and tuple_size(value) == 2 and elem(value, 0) == :char -> ~c"char"
      is_list(value) -> ~c"List"
      is_map(value) -> ~c"Map"
      true -> ~c"unknown"
    end
  end

  @spec string_length_data(term()) :: number()
  def string_length_data(value) do
    length(value)
  end

  @spec string_concat_data(term(), term()) :: term()
  def string_concat_data(left, right) do
    left ++ right
  end

  @spec string_chars_data(term()) :: list()
  def string_chars_data(value) do
    Enum.map(value, fn ch -> {:char, ch} end)
  end

  @spec parse_number_data(term()) :: map()
  def parse_number_data(value) do
    str = any_to_string_data(value) |> to_string()

    case Integer.parse(str) do
      {int, ""} ->
        %{tag: 1, value: int}

      _ ->
        case Float.parse(str) do
          {float, ""} -> %{tag: 1, value: float}
          _ -> %{tag: 0}
        end
    end
  end

  @spec parse_bool_data(term()) :: map()
  def parse_bool_data(value) do
    str = any_to_string_data(value) |> to_string() |> String.downcase()

    case str do
      "true" -> %{tag: 1, value: true}
      "false" -> %{tag: 1, value: false}
      _ -> %{tag: 0}
    end
  end

  @spec parse_char_data(term()) :: map()
  def parse_char_data(value) do
    chars = any_to_string_data(value)

    case chars do
      [ch] -> %{tag: 1, value: {:char, ch}}
      _ -> %{tag: 0}
    end
  end

  @spec string_add_or_numeric(term(), term()) :: term()
  def string_add_or_numeric(left, right) do
    cond do
      string_map?(left) and string_map?(right) ->
        # Both are String objects, call concat
        data = string_concat_data(left.data, right.data)
        %{left | data: data}

      true ->
        # Numeric addition
        left + right
    end
  end

  @spec any_to_string_data(term()) :: charlist()
  def any_to_string_data(value) do
    cond do
      is_integer(value) ->
        Integer.to_charlist(value)

      is_float(value) ->
        Float.to_charlist(value)

      is_boolean(value) ->
        if value, do: ~c"true", else: ~c"false"

      value == :ok ->
        ~c"void"

      string_map?(value) ->
        value.data

      is_tuple(value) and tuple_size(value) == 2 and elem(value, 0) == :char ->
        [elem(value, 1)]

      is_list(value) ->
        # Could be a charlist or list of values
        if Enum.all?(value, &is_integer/1) do
          value
        else
          inspect(value) |> String.to_charlist()
        end

      is_map(value) ->
        inspect(value) |> String.to_charlist()

      true ->
        inspect(value) |> String.to_charlist()
    end
  end

  @spec iterator_next_data(list()) :: map()
  def iterator_next_data([]), do: %{tag: 0}
  def iterator_next_data([head | _]), do: %{tag: 1, value: head}

  @spec iterator_map_data(list(), function()) :: list()
  def iterator_map_data(list, mapper) do
    Enum.map(list, mapper)
  end

  @spec iterator_filter_data(list(), function()) :: list()
  def iterator_filter_data(list, predicate) do
    Enum.filter(list, predicate)
  end

  @spec iterator_fold_data(list(), term(), function()) :: term()
  def iterator_fold_data(list, initial, folder) do
    Enum.reduce(list, initial, folder)
  end

  # List functions
  @spec list_empty() :: list()
  def list_empty(), do: []

  @spec list_length(list()) :: non_neg_integer()
  def list_length(list), do: length(list)

  @spec list_get(list(), integer()) :: map()
  def list_get(list, index) when index >= 0 and index < length(list) do
    %{tag: 1, value: Enum.at(list, index)}
  end
  def list_get(_list, _index), do: %{tag: 0}

  @spec list_push(list(), term()) :: list()
  def list_push(list, item), do: list ++ [item]

  @spec list_pop(list()) :: list()
  def list_pop([]), do: []
  def list_pop(list), do: Enum.drop(list, -1)

  @spec list_first(list()) :: map()
  def list_first([]), do: %{tag: 0}
  def list_first([head | _]), do: %{tag: 1, value: head}

  @spec list_last(list()) :: map()
  def list_last([]), do: %{tag: 0}
  def list_last(list), do: %{tag: 1, value: List.last(list)}

  @spec list_reverse(list()) :: list()
  def list_reverse(list), do: Enum.reverse(list)

  @spec list_concat(list(), list()) :: list()
  def list_concat(left, right), do: left ++ right

  def list_join(lists, separator) do
    sep_data = string_data(separator) |> to_string()
    joined =
      lists
      |> Enum.map(fn lst ->
        lst_data = string_data(lst) |> to_string()
        lst_data
      end)
      |> Enum.join(sep_data)

    stdlib_string_new(joined)
  end

  @spec list_iter(list()) :: map()
  def list_iter(list) do
    apply(:beamlang_program, :iterator_from_list, [list])
  end

  @spec list_map(list(), function()) :: list()
  def list_map(list, mapper) do
    Enum.map(list, mapper)
  end

  @spec list_filter(list(), function()) :: list()
  def list_filter(list, predicate) do
    Enum.filter(list, predicate)
  end

  @spec list_fold(list(), term(), function()) :: term()
  def list_fold(list, initial, folder) do
    Enum.reduce(list, initial, folder)
  end

  @spec list_for_each(list(), function()) :: :ok
  def list_for_each(list, callback) do
    Enum.each(list, callback)
    :ok
  end

  @spec println(term()) :: :ok
  def println(value) do
    printable = string_data(value)
    :io.format("~s~n", [printable])
    :ok
  end

  @spec print(term()) :: :ok
  def print(value) do
    printable = string_data(value)
    :io.format("~s", [printable])
    :ok
  end

  @spec println_raw(term()) :: :ok
  def println_raw(value) do
    IO.inspect(value, limit: :infinity)
    :ok
  end

  # File system operations
  @spec file_read(term()) :: map()
  def file_read(path) do
    path_str = string_data(path) |> to_string()
    case File.read(path_str) do
      {:ok, content} ->
        # Return Result.ok with String struct using stdlib functions
        string_struct = stdlib_string_new(content)
        apply(:beamlang_program, :result_ok, [string_struct])
      {:error, reason} ->
        # Return Result.err using stdlib functions
        error_msg = stdlib_string_new(to_string(reason))
        apply(:beamlang_program, :result_err, [error_msg])
    end
  end

  @spec file_exists(term()) :: boolean()
  def file_exists(path) do
    path_str = string_data(path) |> to_string()
    File.exists?(path_str)
  end

  @spec get_env(term()) :: map()
  def get_env(name) do
    name_str = string_data(name) |> to_string()
    case System.get_env(name_str) do
      nil -> apply(:beamlang_program, :optional_none, [])
      value ->
        string_struct = stdlib_string_new(value)
        apply(:beamlang_program, :optional_some, [string_struct])
    end
  end

  defp string_data(%{__beamlang_type__: "String", data: data}), do: data
  defp string_data(%{__beamlang_type__: ~c"String", data: data}), do: data
  defp string_data(value) when is_map(value) do
    if string_map?(value), do: Map.get(value, :data), else: inspect(value)
  end
  defp string_data(value) when is_list(value), do: value
  defp string_data(value) when is_binary(value), do: value
  defp string_data(value), do: inspect(value)

  defp ensure_charlist(value) when is_binary(value), do: String.to_charlist(value)
  defp ensure_charlist(value) when is_map(value) do
    if string_map?(value), do: Map.get(value, :data), else: value
  end
  defp ensure_charlist(value), do: value

  defp string_map?(value) do
    is_map(value) and Map.has_key?(value, :data) and Map.has_key?(value, :length) and Map.has_key?(value, :chars)
  end

  @doc """
  Parses command line arguments into a struct.

  fields: list of field names (as strings)
  field_types: list of field type atoms (:String, :number, :bool, :char)
  type_label: the BeamLang type name for the result struct
  args: the BeamLang List of string arguments
  """
  @spec parse_args(list(), list(), charlist(), map()) :: map()
  def parse_args(fields, field_types, type_label, args) do
    args_data = args.data
    field_strs = Enum.map(fields, &to_string/1)

    if length(args_data) != length(fields) do
      missing_fields = Enum.drop(field_strs, length(args_data))
      error_struct = %{
        message: stdlib_string_new("Expected #{length(fields)} arguments, got #{length(args_data)}."),
        missing: wrap_as_beamlang_list(Enum.map(missing_fields, &stdlib_string_new/1)),
        __beamlang_type__: ~c"ArgsError"
      }
      apply(:beamlang_program, :optional_none, []) |> Map.put(:value, error_struct)
    else
      parse_args_fields(fields, field_types, args_data, type_label, %{})
    end
  end

  defp parse_args_fields([], [], _args, type_label, acc) do
    result = Map.put(acc, :__beamlang_type__, type_label)
    %{tag: 1, value: result}
  end

  defp parse_args_fields([field | rest_fields], [type | rest_types], [arg | rest_args], type_label, acc) do
    # field comes as charlist from Erlang, convert to string then atom
    field_str = to_string(field)
    case parse_arg_value(arg, type, field_str) do
      {:ok, value} ->
        acc = Map.put(acc, String.to_atom(field_str), value)
        parse_args_fields(rest_fields, rest_types, rest_args, type_label, acc)

      {:error, error_struct} ->
        %{tag: 0, value: error_struct}
    end
  end

  defp parse_arg_value(arg, :String, _field) do
    {:ok, to_string_struct(arg)}
  end

  defp parse_arg_value(arg, {:named, "String"}, field) do
    parse_arg_value(arg, :String, field)
  end

  defp parse_arg_value(arg, :number, field) do
    case parse_number_data(arg) do
      %{tag: 1, value: num} -> {:ok, num}
      %{tag: 0} -> {:error, args_error_struct(field, "number")}
    end
  end

  defp parse_arg_value(arg, {:named, "number"}, field) do
    parse_arg_value(arg, :number, field)
  end

  defp parse_arg_value(arg, :bool, field) do
    case parse_bool_data(arg) do
      %{tag: 1, value: b} -> {:ok, b}
      %{tag: 0} -> {:error, args_error_struct(field, "bool")}
    end
  end

  defp parse_arg_value(arg, {:named, "bool"}, field) do
    parse_arg_value(arg, :bool, field)
  end

  defp parse_arg_value(arg, :char, field) do
    case parse_char_data(arg) do
      %{tag: 1, value: ch} -> {:ok, ch}
      %{tag: 0} -> {:error, args_error_struct(field, "char")}
    end
  end

  defp parse_arg_value(arg, {:named, "char"}, field) do
    parse_arg_value(arg, :char, field)
  end

  defp parse_arg_value(_arg, _type, field) do
    {:error, args_error_struct(field, "unknown")}
  end

  defp args_error_struct(field, expected) do
    %{
      message: stdlib_string_new("Invalid value for #{field} (expected #{expected})."),
      missing: wrap_as_beamlang_list([]),
      __beamlang_type__: ~c"ArgsError"
    }
  end

  defp to_string_struct(value) when is_map(value) do
    if string_map?(value), do: value, else: stdlib_string_new(inspect(value))
  end

  defp to_string_struct(value) do
    stdlib_string_new(any_to_string_data(value))
  end

  # Delegates to the compiled stdlib's string_new function
  defp stdlib_string_new(data) when is_list(data) do
    apply(:beamlang_program, :string_new, [data])
  end

  defp stdlib_string_new(data) when is_binary(data) do
    stdlib_string_new(String.to_charlist(data))
  end
end
