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
  This mirrors the structure created by stdlib/list.bl's list_from_data.
  """
  @spec wrap_as_beamlang_list(list()) :: map()
  def wrap_as_beamlang_list(data) when is_list(data) do
    %{
      data: data,
      length: &list_length_method/1,
      get: &list_get_method/2,
      push: &list_push_method/2,
      pop: &list_pop_method/1,
      first: &list_first_method/1,
      last: &list_last_method/1,
      iter: &list_iter_method/1,
      map: &list_map_method/2,
      filter: &list_filter_method/2,
      fold: &list_fold_method/3,
      reverse: &list_reverse_method/1,
      concat: &list_concat_method/2,
      __beamlang_type__: ~c"List"
    }
  end

  defp list_length_method(self), do: list_length(self.data)
  defp list_get_method(self, index), do: list_get(self.data, index)
  defp list_push_method(self, item), do: wrap_as_beamlang_list(list_push(self.data, item))
  defp list_pop_method(self), do: wrap_as_beamlang_list(list_pop(self.data))
  defp list_first_method(self), do: list_first(self.data)
  defp list_last_method(self), do: list_last(self.data)
  defp list_iter_method(self), do: list_iter(self.data)
  defp list_map_method(self, mapper), do: wrap_as_beamlang_list(list_map(self.data, mapper))
  defp list_filter_method(self, pred), do: wrap_as_beamlang_list(list_filter(self.data, pred))
  defp list_fold_method(self, init, folder), do: list_fold(self.data, init, folder)
  defp list_reverse_method(self), do: wrap_as_beamlang_list(list_reverse(self.data))
  defp list_concat_method(self, other), do: wrap_as_beamlang_list(list_concat(self.data, other.data))

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

  @spec list_iter(list()) :: map()
  def list_iter(list) do
    %{
      data: list,
      next: fn data -> iterator_next_data(data) end,
      map: fn data, mapper -> iterator_map_data(data, mapper) end,
      fold: fn data, init, folder -> iterator_fold_data(data, init, folder) end,
      filter: fn data, predicate -> iterator_filter_data(data, predicate) end,
      __beamlang_type__: ~c"Iterator<any>"
    }
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

  # File system operations
  @spec file_read(term()) :: map()
  def file_read(path) do
    path_str = string_data(path) |> to_string()
    case File.read(path_str) do
      {:ok, content} ->
        # Return Result.ok with String struct (tag=1 for ok)
        string_struct = %{
          data: String.to_charlist(content),
          length: &string_length_data/1,
          concat: &string_concat_data/2,
          chars: &string_chars_data/1,
          __beamlang_type__: ~c"String"
        }
        %{kind: 2, tag: 1, value: string_struct}
      {:error, reason} ->
        # Return Result.err (tag=0 for err, value contains error)
        error_msg = %{
          data: String.to_charlist(to_string(reason)),
          length: &string_length_data/1,
          concat: &string_concat_data/2,
          chars: &string_chars_data/1,
          __beamlang_type__: ~c"String"
        }
        %{kind: 2, tag: 0, value: error_msg}
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
      nil -> %{tag: 0}
      value ->
        string_struct = %{
          data: String.to_charlist(value),
          length: &string_length_data/1,
          concat: &string_concat_data/2,
          chars: &string_chars_data/1,
          __beamlang_type__: ~c"String"
        }
        %{tag: 1, value: string_struct}
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
end
