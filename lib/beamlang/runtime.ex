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

  @spec load_and_run(atom(), binary()) :: {:ok, term()} | {:error, map()}
  def load_and_run(module, binary) do
    case :code.load_binary(module, ~c"beamlang_program.beam", binary) do
      {:module, ^module} -> {:ok, apply(module, :main, [])}
      {:error, reason} -> {:error, %{message: "Failed to load compiled module: #{inspect(reason)}"}}
    end
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
