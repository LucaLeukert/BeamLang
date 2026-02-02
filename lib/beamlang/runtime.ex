defmodule BeamLang.Runtime do
  @moduledoc """
  Compiles Erlang abstract forms to BEAM and executes main/0.
  """

  alias BeamLang.Codegen

  # Get the current BeamLang module (set during load_and_run)
  defp current_module, do: Process.get(:beamlang_module, :beamlang_program)

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
        # Store the current module in process dictionary for Runtime functions
        Process.put(:beamlang_module, module)
        # Convert raw Erlang list to BeamLang List struct using the loaded module
        beamlang_list = wrap_as_beamlang_list(args, module)
        {:ok, apply(module, :main, [beamlang_list])}
      {:error, reason} -> {:error, %{message: "Failed to load compiled module: #{inspect(reason)}"}}
    end
  end

  @doc """
  Wraps a raw Erlang list into a BeamLang List struct.
  Delegates to the compiled stdlib's list_from_data function.
  For string args, also wraps each element as a BeamLang String.
  """
  @spec wrap_as_beamlang_list(list(), atom()) :: map()
  def wrap_as_beamlang_list(data, module \\ :beamlang_program) when is_list(data) do
    # Wrap each string element as a BeamLang String
    wrapped_data = Enum.map(data, fn elem ->
      if is_list(elem) do
        # It's a charlist - wrap as BeamLang String
        apply(module, :string_new, [elem])
      else
        elem
      end
    end)
    apply(module, :list_from_data, [wrapped_data])
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

  # New String methods

  @spec string_split_data(term(), term()) :: list()
  def string_split_data(value, separator) do
    str = to_string(value)
    sep = to_string(separator)
    String.split(str, sep)
    |> Enum.map(&String.to_charlist/1)
  end

  @spec string_contains_data(term(), term()) :: boolean()
  def string_contains_data(value, needle) do
    String.contains?(to_string(value), to_string(needle))
  end

  @spec string_starts_with_data(term(), term()) :: boolean()
  def string_starts_with_data(value, prefix) do
    String.starts_with?(to_string(value), to_string(prefix))
  end

  @spec string_ends_with_data(term(), term()) :: boolean()
  def string_ends_with_data(value, suffix) do
    String.ends_with?(to_string(value), to_string(suffix))
  end

  @spec string_trim_data(term()) :: term()
  def string_trim_data(value) do
    String.trim(to_string(value)) |> String.to_charlist()
  end

  @spec string_trim_start_data(term()) :: term()
  def string_trim_start_data(value) do
    String.trim_leading(to_string(value)) |> String.to_charlist()
  end

  @spec string_trim_end_data(term()) :: term()
  def string_trim_end_data(value) do
    String.trim_trailing(to_string(value)) |> String.to_charlist()
  end

  @spec string_replace_data(term(), term(), term()) :: term()
  def string_replace_data(value, pattern, replacement) do
    String.replace(to_string(value), to_string(pattern), to_string(replacement))
    |> String.to_charlist()
  end

  @spec string_to_upper_data(term()) :: term()
  def string_to_upper_data(value) do
    String.upcase(to_string(value)) |> String.to_charlist()
  end

  @spec string_to_lower_data(term()) :: term()
  def string_to_lower_data(value) do
    String.downcase(to_string(value)) |> String.to_charlist()
  end

  @spec string_substring_data(term(), integer(), integer()) :: term()
  def string_substring_data(value, start_idx, end_idx) do
    str = to_string(value)
    len = end_idx - start_idx
    String.slice(str, start_idx, len) |> String.to_charlist()
  end

  @spec string_index_of_data(term(), term()) :: map()
  def string_index_of_data(value, needle) do
    str = to_string(value)
    needle_str = to_string(needle)
    case :binary.match(str, needle_str) do
      {idx, _} -> %{tag: 1, value: idx}
      :nomatch -> %{tag: 0}
    end
  end

  @spec list_map_to_strings(list()) :: list()
  def list_map_to_strings(list) do
    Enum.map(list, fn data -> stdlib_string_new(data) end)
  end

  @spec optional_from_data(map()) :: map()
  def optional_from_data(%{tag: 1, value: value}) do
    apply(current_module(), :optional_some, [value])
  end
  def optional_from_data(%{tag: 0}) do
    apply(current_module(), :optional_none, [])
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
      is_binary(value) ->
        # Elixir binary string -> charlist
        String.to_charlist(value)

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

  # Helpers for wrapping raw optionals
  @spec optional_raw_tag(map()) :: integer()
  def optional_raw_tag(%{tag: tag}), do: tag
  def optional_raw_tag(_), do: 0

  @spec optional_raw_value(map()) :: any()
  def optional_raw_value(%{value: value}), do: value
  def optional_raw_value(_), do: nil

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
    apply(current_module(), :iterator_from_list, [list])
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
        apply(current_module(), :result_ok, [string_struct])
      {:error, reason} ->
        # Return Result.err using stdlib functions
        error_msg = stdlib_string_new(to_string(reason))
        apply(current_module(), :result_err, [error_msg])
    end
  end

  @spec file_exists(term()) :: boolean()
  def file_exists(path) do
    path_str = string_data(path) |> to_string()
    File.exists?(path_str)
  end

  @spec list_directory(term()) :: map()
  def list_directory(path) do
    path_str = string_data(path) |> to_string()
    case File.ls(path_str) do
      {:ok, entries} ->
        # Create a list of maps with name, is_dir, and size
        entries_list = Enum.flat_map(entries, fn entry ->
          full_path = Path.join(path_str, entry)
          case File.stat(full_path, time: :posix) do
            {:ok, stat} ->
              is_dir = stat.type == :directory
              size = stat.size
              name_string = stdlib_string_new(entry)
              
              # Use BeamLang stdlib function to construct FileEntry
              file_entry = apply(current_module(), :file_entry_new, [name_string, is_dir, size])
              [file_entry]
            {:error, _reason} ->
              # Skip files that cannot be stat'd (deleted, permission denied, etc.)
              []
          end
        end)
        
        # Convert to BeamLang list and wrap in Result.ok
        beamlang_list = apply(current_module(), :list_from_data, [entries_list])
        apply(current_module(), :result_ok, [beamlang_list])
      {:error, reason} ->
        # Return Result.err using stdlib functions
        error_msg = stdlib_string_new(to_string(reason))
        apply(current_module(), :result_err, [error_msg])
    end
  end

  @spec read_stdin() :: map()
  def read_stdin() do
    case IO.read(:stdio, :eof) do
      data when is_binary(data) ->
        stdlib_string_new(data)
      _ ->
        stdlib_string_new("")
    end
  end

  @spec get_env(term()) :: map()
  def get_env(name) do
    name_str = string_data(name) |> to_string()
    case System.get_env(name_str) do
      nil -> apply(current_module(), :optional_none, [])
      value ->
        string_struct = stdlib_string_new(value)
        apply(current_module(), :optional_some, [string_struct])
    end
  end

  @spec clock_now() :: number()
  def clock_now() do
    :erlang.monotonic_time(:millisecond)
  end

  defp string_data(%{__beamlang_type__: "String", data: data}), do: data
  defp string_data(%{__beamlang_type__: ~c"String", data: data}), do: data
  defp string_data(value) when is_map(value) do
    if string_map?(value), do: Map.get(value, :data), else: inspect(value)
  end
  defp string_data(value) when is_list(value), do: value
  defp string_data(value) when is_binary(value), do: value
  defp string_data({:char, code}) when is_integer(code), do: [code]
  defp string_data(value), do: inspect(value)

  defp ensure_charlist(value) when is_binary(value), do: String.to_charlist(value)
  defp ensure_charlist(value) when is_map(value) do
    if string_map?(value), do: Map.get(value, :data), else: value
  end
  defp ensure_charlist(value), do: value

  defp string_map?(value) do
    is_map(value) and Map.has_key?(value, :data) and Map.has_key?(value, :length) and Map.has_key?(value, :chars)
  end

  @spec parse_args(list(), list(), list(), charlist(), map()) :: map()
  def parse_args(fields, field_types, field_annotations, type_label, args) do
    args_data = args.data
    field_strs = Enum.map(fields, &to_string/1)
    module = current_module()

    # Build field specs from annotations
    field_specs = build_field_specs(field_strs, field_types, field_annotations)

    # Parse the args with annotation support
    case parse_args_with_annotations(args_data, field_specs, type_label, module) do
      {:ok, result} -> %{tag: 1, value: result}
      {:error, error_struct} -> %{tag: 0, value: error_struct}
    end
  end

  # Build field specifications from annotations
  defp build_field_specs(fields, types, annotations) do
    Enum.zip([fields, types, annotations])
    |> Enum.map(fn {field, type, anns} ->
      ann_map = parse_annotations(anns)
      %{
        name: field,
        type: type,
        # If no annotations at all, treat as implicitly required (backwards compatibility)
        # Otherwise, check for explicit @required annotation
        required: if anns == nil or anns == [] do true else Map.get(ann_map, "required", false) end,
        default: Map.get(ann_map, "default"),
        description: Map.get(ann_map, "description"),
        short: Map.get(ann_map, "short"),
        long: Map.get(ann_map, "long"),
        flag: Map.get(ann_map, "flag", false),
        positional: !Map.has_key?(ann_map, "short") and !Map.has_key?(ann_map, "long") and !Map.get(ann_map, "flag", false),
        # Track whether this field has any annotations (for backwards compatibility)
        has_annotations: anns != nil and anns != []
      }
    end)
  end

  # Parse annotation list into a map
  defp parse_annotations(nil), do: %{}
  defp parse_annotations(anns) when is_list(anns) do
    Enum.reduce(anns, %{}, fn
      {name, args}, acc when is_list(name) ->
        name_str = to_string(name)
        case args do
          [] -> Map.put(acc, name_str, true)
          [value] -> Map.put(acc, name_str, annotation_value(value))
          values -> Map.put(acc, name_str, Enum.map(values, &annotation_value/1))
        end
      _, acc -> acc
    end)
  end

  defp annotation_value(v) when is_list(v), do: to_string(v)
  defp annotation_value(v), do: v

  # Parse args with annotation support
  defp parse_args_with_annotations(args_data, field_specs, type_label, module) do
    # Separate named/flag args from positional args
    {named_specs, positional_specs} = Enum.split_with(field_specs, fn spec -> !spec.positional end)

    # Parse named/flag arguments first
    {remaining_args, named_values} = parse_named_args(args_data, named_specs, %{})

    # Parse positional arguments
    case parse_positional_args(remaining_args, positional_specs, named_values) do
      {:ok, values} ->
        # Check required fields and apply defaults
        case apply_defaults_and_check_required(values, field_specs, type_label, module) do
          {:ok, result} -> {:ok, result}
          {:error, _} = err -> err
        end
      {:error, _} = err -> err
    end
  end

  # Parse named arguments (--name=value, --name value, -n value, -n=value)
  # This function now scans through all args and picks out the named ones,
  # leaving positional args for later processing
  defp parse_named_args(args, specs, acc) do
    do_parse_named_args(args, specs, acc, [])
  end

  defp do_parse_named_args([], _specs, acc, positional_acc) do
    {Enum.reverse(positional_acc), acc}
  end
  defp do_parse_named_args([arg | rest] = _args, specs, acc, positional_acc) do
    arg_str = string_value(arg)

    cond do
      # Long form: --name=value or --name value
      String.starts_with?(arg_str, "--") ->
        case parse_long_arg(arg_str, rest, specs) do
          {:ok, field_name, value, remaining} ->
            do_parse_named_args(remaining, specs, Map.put(acc, field_name, value), positional_acc)
          :not_matched ->
            # Unrecognized flag, treat as positional
            do_parse_named_args(rest, specs, acc, [arg | positional_acc])
        end

      # Short form: -n=value or -n value
      String.starts_with?(arg_str, "-") and String.length(arg_str) >= 2 ->
        case parse_short_arg(arg_str, rest, specs) do
          {:ok, field_name, value, remaining} ->
            do_parse_named_args(remaining, specs, Map.put(acc, field_name, value), positional_acc)
          :not_matched ->
            # Unrecognized flag, treat as positional
            do_parse_named_args(rest, specs, acc, [arg | positional_acc])
        end

      true ->
        # Positional argument, continue collecting
        do_parse_named_args(rest, specs, acc, [arg | positional_acc])
    end
  end

  defp parse_long_arg(arg_str, rest, specs) do
    # Remove "--" prefix
    without_prefix = String.slice(arg_str, 2, String.length(arg_str) - 2)

    # Check for --name=value form
    case String.split(without_prefix, "=", parts: 2) do
      [name, value] ->
        case find_spec_by_long(name, specs) do
          nil -> :not_matched
          spec ->
            parsed_value = if spec.flag, do: true, else: value
            {:ok, spec.name, parsed_value, rest}
        end
      [name] ->
        case find_spec_by_long(name, specs) do
          nil -> :not_matched
          spec ->
            if spec.flag do
              {:ok, spec.name, true, rest}
            else
              case rest do
                [next_arg | remaining] ->
                  {:ok, spec.name, string_value(next_arg), remaining}
                [] ->
                  :not_matched
              end
            end
        end
    end
  end

  defp parse_short_arg(arg_str, rest, specs) do
    # Remove "-" prefix
    without_prefix = String.slice(arg_str, 1, String.length(arg_str) - 1)

    # Check for -n=value form
    case String.split(without_prefix, "=", parts: 2) do
      [name, value] when byte_size(name) == 1 ->
        case find_spec_by_short(name, specs) do
          nil -> :not_matched
          spec ->
            parsed_value = if spec.flag, do: true, else: value
            {:ok, spec.name, parsed_value, rest}
        end
      [name] when byte_size(name) == 1 ->
        case find_spec_by_short(name, specs) do
          nil -> :not_matched
          spec ->
            if spec.flag do
              {:ok, spec.name, true, rest}
            else
              case rest do
                [next_arg | remaining] ->
                  {:ok, spec.name, string_value(next_arg), remaining}
                [] ->
                  :not_matched
              end
            end
        end
      _ ->
        :not_matched
    end
  end

  defp find_spec_by_long(name, specs) do
    Enum.find(specs, fn spec ->
      spec.long == name or (spec.long == nil and spec.name == name)
    end)
  end

  defp find_spec_by_short(name, specs) do
    Enum.find(specs, fn spec -> spec.short == name end)
  end

  # Parse positional arguments
  defp parse_positional_args(args, specs, acc) do
    case {args, specs} do
      {[], []} ->
        {:ok, acc}
      {[], _remaining_specs} ->
        # Some specs not filled, will be handled by defaults
        {:ok, acc}
      {[arg | rest_args], [spec | rest_specs]} ->
        value = string_value(arg)
        parse_positional_args(rest_args, rest_specs, Map.put(acc, spec.name, value))
      {_extra_args, []} ->
        # Extra args, ignore them
        {:ok, acc}
    end
  end

  # Apply defaults and check required fields
  defp apply_defaults_and_check_required(values, field_specs, type_label, module) do
    result = Enum.reduce_while(field_specs, %{}, fn spec, acc ->
      case Map.fetch(values, spec.name) do
        {:ok, raw_value} ->
          # Parse the value according to type
          case parse_arg_value(raw_value, spec.type, spec.name) do
            {:ok, parsed} -> {:cont, Map.put(acc, String.to_atom(spec.name), parsed)}
            {:error, _} = err -> {:halt, err}
          end
        :error ->
          # Value not provided, check for default
          cond do
            spec.default != nil ->
              # Use default value
              case parse_arg_value(spec.default, spec.type, spec.name) do
                {:ok, parsed} -> {:cont, Map.put(acc, String.to_atom(spec.name), parsed)}
                {:error, _} = err -> {:halt, err}
              end
            spec.flag ->
              # Flags default to false
              {:cont, Map.put(acc, String.to_atom(spec.name), false)}
            spec.required ->
              # Required field missing
              error_struct = %{
                message: stdlib_string_new("Missing required argument: #{spec.name}"),
                missing: wrap_as_beamlang_list([stdlib_string_new(spec.name)], module),
                __beamlang_type__: ~c"ArgsError"
              }
              {:halt, {:error, error_struct}}
            true ->
              # Not required and no default - return error for missing
              error_struct = %{
                message: stdlib_string_new("Missing argument: #{spec.name}"),
                missing: wrap_as_beamlang_list([stdlib_string_new(spec.name)], module),
                __beamlang_type__: ~c"ArgsError"
              }
              {:halt, {:error, error_struct}}
          end
      end
    end)

    case result do
      {:error, _} = err -> err
      values_map ->
        {:ok, Map.put(values_map, :__beamlang_type__, type_label)}
    end
  end

  defp string_value(value) when is_map(value) do
    if string_map?(value), do: to_string(value.data), else: inspect(value)
  end
  defp string_value(value) when is_list(value), do: to_string(value)
  defp string_value(value) when is_binary(value), do: value
  defp string_value(value), do: inspect(value)

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
    # Handle already-boolean values (from flags)
    if is_boolean(arg) do
      {:ok, arg}
    else
      case parse_bool_data(arg) do
        %{tag: 1, value: b} -> {:ok, b}
        %{tag: 0} -> {:error, args_error_struct(field, "bool")}
      end
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
    module = current_module()
    %{
      message: stdlib_string_new("Invalid value for #{field} (expected #{expected})."),
      missing: wrap_as_beamlang_list([], module),
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
    apply(current_module(), :string_new, [data])
  end

  defp stdlib_string_new(data) when is_binary(data) do
    stdlib_string_new(String.to_charlist(data))
  end

  # Math functions
  @spec math_sqrt(number()) :: number()
  def math_sqrt(n), do: :math.sqrt(n)

  @spec math_abs(number()) :: number()
  def math_abs(n), do: abs(n)

  @spec math_floor(number()) :: number()
  def math_floor(n), do: :math.floor(n)

  @spec math_ceil(number()) :: number()
  def math_ceil(n), do: :math.ceil(n)

  @spec math_round(number()) :: number()
  def math_round(n), do: round(n)

  @spec math_sin(number()) :: number()
  def math_sin(n), do: :math.sin(n)

  @spec math_cos(number()) :: number()
  def math_cos(n), do: :math.cos(n)

  @spec math_tan(number()) :: number()
  def math_tan(n), do: :math.tan(n)

  @spec math_asin(number()) :: number()
  def math_asin(n), do: :math.asin(n)

  @spec math_acos(number()) :: number()
  def math_acos(n), do: :math.acos(n)

  @spec math_atan(number()) :: number()
  def math_atan(n), do: :math.atan(n)

  @spec math_atan2(number(), number()) :: number()
  def math_atan2(y, x), do: :math.atan2(y, x)

  @spec math_pow(number(), number()) :: number()
  def math_pow(base, exp), do: :math.pow(base, exp)

  @spec math_log(number()) :: number()
  def math_log(n), do: :math.log(n)

  @spec math_log10(number()) :: number()
  def math_log10(n), do: :math.log10(n)

  @spec math_exp(number()) :: number()
  def math_exp(n), do: :math.exp(n)

  @spec math_pi() :: number()
  def math_pi(), do: :math.pi()

  @spec math_e() :: number()
  def math_e(), do: :math.exp(1)

  # Range functions
  @spec range_to_list(number(), number(), number()) :: list()
  def range_to_list(start, stop, step) when step > 0 do
    Enum.take_while(Stream.iterate(start, &(&1 + step)), &(&1 < stop))
  end
  def range_to_list(start, stop, step) when step < 0 do
    Enum.take_while(Stream.iterate(start, &(&1 + step)), &(&1 > stop))
  end
  def range_to_list(start, _stop, 0), do: [start]

  # Map functions
  @spec map_new() :: map()
  def map_new(), do: %{}

  @spec map_get(map(), term()) :: map()
  def map_get(data, key) do
    case Map.fetch(data, key) do
      {:ok, value} -> %{tag: 1, value: value}
      :error -> %{tag: 0}
    end
  end

  @spec map_put(map(), term(), term()) :: map()
  def map_put(data, key, value), do: Map.put(data, key, value)

  @spec map_remove(map(), term()) :: map()
  def map_remove(data, key), do: Map.delete(data, key)

  @spec map_contains(map(), term()) :: boolean()
  def map_contains(data, key), do: Map.has_key?(data, key)

  @spec map_size_data(map()) :: non_neg_integer()
  def map_size_data(data), do: Kernel.map_size(data)

  @spec map_keys(map()) :: list()
  def map_keys(data), do: Map.keys(data)

  @spec map_values(map()) :: list()
  def map_values(data), do: Map.values(data)

  @spec map_entries(map()) :: list()
  def map_entries(data) do
    Enum.map(data, fn {k, v} -> %{first: k, second: v} end)
  end

  def set_new(), do: MapSet.new()

  @spec set_add(MapSet.t(), term()) :: MapSet.t()
  def set_add(data, item), do: MapSet.put(data, item)

  @spec set_remove(MapSet.t(), term()) :: MapSet.t()
  def set_remove(data, item), do: MapSet.delete(data, item)

  @spec set_contains(MapSet.t(), term()) :: boolean()
  def set_contains(data, item), do: MapSet.member?(data, item)

  @spec set_size(MapSet.t()) :: non_neg_integer()
  def set_size(data), do: MapSet.size(data)

  @spec set_to_list(MapSet.t()) :: list()
  def set_to_list(data), do: MapSet.to_list(data)

  @spec set_union(MapSet.t(), MapSet.t()) :: MapSet.t()
  def set_union(a, b), do: MapSet.union(a, b)

  @spec set_intersection(MapSet.t(), MapSet.t()) :: MapSet.t()
  def set_intersection(a, b), do: MapSet.intersection(a, b)

  @spec set_difference(MapSet.t(), MapSet.t()) :: MapSet.t()
  def set_difference(a, b), do: MapSet.difference(a, b)
end
