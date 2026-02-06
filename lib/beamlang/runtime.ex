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
      {:ok, module, binary} ->
        {:ok, module, binary}

      {:ok, module, binary, _warnings} ->
        {:ok, module, binary}

      {:error, errors, warnings} ->
        {:error, %{message: "BEAM compile failed: #{inspect({errors, warnings})}"}}
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

      {:error, reason} ->
        {:error, %{message: "Failed to load compiled module: #{inspect(reason)}"}}
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
    wrapped_data =
      Enum.map(data, fn elem ->
        cond do
          is_list(elem) ->
            # It's a charlist - wrap as BeamLang String
            apply(module, :string_new, [elem])

          is_binary(elem) ->
            # It's an Elixir binary string - convert to charlist and wrap
            apply(module, :string_new, [String.to_charlist(elem)])

          true ->
            elem
        end
      end)

    apply(module, :list_from_data, [wrapped_data])
  end

  @spec load_modules([{atom(), binary()}]) :: :ok | {:error, map()}
  def load_modules(modules) when is_list(modules) do
    Enum.reduce_while(modules, :ok, fn {module, binary}, _acc ->
      case :code.load_binary(module, ~c"beamlang_module.beam", binary) do
        {:module, ^module} ->
          {:cont, :ok}

        {:error, reason} ->
          {:halt, {:error, %{message: "Failed to load compiled module: #{inspect(reason)}"}}}
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
      is_integer(value) or is_float(value) ->
        ~c"number"

      is_binary(value) ->
        ~c"String"

      is_boolean(value) ->
        ~c"bool"

      value == :ok ->
        ~c"void"

      is_map(value) and Map.has_key?(value, :__beamlang_type__) ->
        ensure_charlist(Map.get(value, :__beamlang_type__))

      is_map(value) and Map.get(value, :kind) == 1 ->
        ~c"Optional"

      is_map(value) and Map.get(value, :kind) == 2 ->
        ~c"Result"

      is_map(value) and string_map?(value) ->
        ~c"String"

      is_tuple(value) and tuple_size(value) == 2 and elem(value, 0) == :char ->
        ~c"char"

      is_list(value) ->
        ~c"List"

      is_map(value) ->
        ~c"Map"

      true ->
        ~c"unknown"
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
    split_list = String.split(str, sep)
    string_list = Enum.map(split_list, &stdlib_string_new/1)
    apply(current_module(), :list_from_data, [string_list])
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
    value
    |> format_value_iodata()
    |> IO.iodata_to_binary()
    |> String.to_charlist()
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
    printable = any_to_string_data(value)
    :io.format("~s~n", [printable])
    :ok
  end

  @spec print(term()) :: :ok
  def print(value) do
    printable = any_to_string_data(value)
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

  @spec file_write(term(), term()) :: map()
  def file_write(path, contents) do
    path_str = string_data(path) |> to_string()
    data = string_data(contents)

    case File.write(path_str, data) do
      :ok ->
        apply(current_module(), :result_ok, [true])

      {:error, reason} ->
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
        entries_list =
          Enum.flat_map(entries, fn entry ->
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

  @spec http_request(term(), term(), term(), term(), number(), boolean()) :: map()
  def http_request(method, url, headers, body, timeout_ms, follow_redirects) do
    ensure_http_client()

    case http_method_atom(method) do
      :unknown ->
        error_msg = stdlib_string_new("Unsupported HTTP method.")
        apply(current_module(), :result_err, [error_msg])

      method_atom ->
        url_data = string_data(url)

        url_charlist =
          if is_list(url_data), do: url_data, else: to_string(url_data) |> String.to_charlist()

        {header_tuples, content_type} = http_headers(headers)
        body_data = string_data(body)
        has_body = body_data != []

        request =
          if method_atom in [:post, :put, :patch] or has_body do
            {url_charlist, header_tuples, content_type, body_data}
          else
            {url_charlist, header_tuples}
          end

        timeout = normalize_timeout(timeout_ms)
        http_options = [timeout: timeout, connect_timeout: timeout]
        options = if follow_redirects, do: [autoredirect: true], else: []

        case :httpc.request(method_atom, request, http_options, options) do
          {:ok, {{_version, status, _reason}, resp_headers, resp_body}} ->
            response = http_response_struct(status, resp_headers, resp_body)
            apply(current_module(), :result_ok, [response])

          {:ok, {status_line, resp_headers, resp_body}} ->
            status = parse_status_line(status_line)
            response = http_response_struct(status, resp_headers, resp_body)
            apply(current_module(), :result_ok, [response])

          {:error, reason} ->
            error_msg = stdlib_string_new(to_string(reason))
            apply(current_module(), :result_err, [error_msg])
        end
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
      nil ->
        apply(current_module(), :optional_none, [])

      value ->
        string_struct = stdlib_string_new(value)
        apply(current_module(), :optional_some, [string_struct])
    end
  end

  @spec clock_now() :: number()
  def clock_now() do
    :erlang.monotonic_time(:millisecond)
  end

  defp ensure_http_client do
    _ = Application.ensure_all_started(:inets)
    _ = Application.ensure_all_started(:ssl)
    :ok
  end

  defp http_method_atom(method) do
    method_str = string_data(method) |> to_string() |> String.upcase()

    case method_str do
      "GET" -> :get
      "POST" -> :post
      "PUT" -> :put
      "PATCH" -> :patch
      "DELETE" -> :delete
      "HEAD" -> :head
      "OPTIONS" -> :options
      "TRACE" -> :trace
      "CONNECT" -> :connect
      _ -> :unknown
    end
  end

  defp normalize_timeout(timeout_ms) do
    cond do
      is_number(timeout_ms) and timeout_ms > 0 ->
        round(timeout_ms)

      true ->
        30_000
    end
  end

  defp http_headers(headers) do
    header_items = list_data(headers)

    {tuples, content_type} =
      Enum.reduce(header_items, {[], "application/octet-stream"}, fn header,
                                                                     {acc, content_type} ->
        header_str = string_data(header) |> to_string()

        case String.split(header_str, ":", parts: 2) do
          [name, value] ->
            key = String.trim(name)
            val = String.trim(value)
            key_downcase = String.downcase(key)
            content_type = if key_downcase == "content-type", do: val, else: content_type
            {[{String.to_charlist(key), String.to_charlist(val)} | acc], content_type}

          _ ->
            {acc, content_type}
        end
      end)

    {Enum.reverse(tuples), content_type}
  end

  defp http_response_struct(status, resp_headers, resp_body) do
    body_binary = IO.iodata_to_binary(resp_body)
    body_struct = stdlib_string_new(body_binary)

    headers =
      resp_headers
      |> Enum.map(fn {key, value} ->
        line = "#{to_string(key)}: #{to_string(value)}"
        stdlib_string_new(line)
      end)

    header_list = wrap_as_beamlang_list(headers, current_module())
    apply(current_module(), :http_response_new, [status, body_struct, header_list])
  end

  defp parse_status_line({_http, status, _reason}) when is_integer(status), do: status
  defp parse_status_line({_http, status, _reason, _}) when is_integer(status), do: status
  defp parse_status_line(_), do: 0

  defp string_data(%{__beamlang_type__: "String", data: data}), do: data
  defp string_data(%{__beamlang_type__: ~c"String", data: data}), do: data

  defp string_data(value) when is_map(value) do
    if string_map?(value), do: Map.get(value, :data), else: inspect(value)
  end

  defp string_data(value) when is_list(value), do: value
  defp string_data(value) when is_binary(value), do: value
  defp string_data({:char, code}) when is_integer(code), do: [code]
  defp string_data(value), do: inspect(value)

  defp format_value_iodata(value) do
    cond do
      is_binary(value) ->
        value

      is_integer(value) ->
        Integer.to_string(value)

      is_float(value) ->
        Float.to_string(value)

      is_boolean(value) ->
        if value, do: "true", else: "false"

      value == :ok ->
        "void"

      string_map?(value) ->
        Map.get(value, :data)

      is_tuple(value) and tuple_size(value) == 2 and elem(value, 0) == :char ->
        [elem(value, 1)]

      is_function(value) ->
        "<fn>"

      is_list(value) ->
        format_list_iodata(value)

      is_map(value) ->
        format_map_iodata(value)

      true ->
        inspect(value)
    end
  end

  defp format_list_iodata(list) do
    if charlist?(list) do
      list
    else
      items = Enum.map(list, &format_value_iodata/1)
      ["[", Enum.intersperse(items, ", "), "]"]
    end
  end

  defp format_map_iodata(map) do
    cond do
      Map.has_key?(map, :__beamlang_type__) ->
        format_struct_iodata(map)

      optional_map?(map) ->
        format_optional_iodata(map)

      result_map?(map) ->
        format_result_iodata(map)

      true ->
        format_plain_map_iodata(map)
    end
  end

  defp format_struct_iodata(map) do
    type_label = ensure_charlist(Map.get(map, :__beamlang_type__))
    type_name = to_string(type_label)

    fields =
      map
      |> Map.delete(:__beamlang_type__)
      |> Enum.reject(fn {key, _value} -> operator_field?(key) end)
      |> Enum.sort_by(fn {key, _value} -> map_key_sort_key(key) end)

    field_items =
      Enum.map(fields, fn {key, value} ->
        [format_field_name(key), " = ", format_value_iodata(value)]
      end)

    [type_name, "{", Enum.intersperse(field_items, ", "), "}"]
  end

  defp format_optional_iodata(map) do
    case Map.get(map, :tag) do
      1 -> ["?some ", format_value_iodata(Map.get(map, :value))]
      _ -> "?none"
    end
  end

  defp format_result_iodata(map) do
    case Map.get(map, :tag) do
      1 -> ["!ok ", format_value_iodata(Map.get(map, :value))]
      _ -> ["!err ", format_value_iodata(Map.get(map, :value))]
    end
  end

  defp format_plain_map_iodata(map) do
    entries =
      map
      |> Enum.sort_by(fn {key, _value} -> map_key_sort_key(key) end)
      |> Enum.map(fn {key, value} ->
        [format_map_key(key), ": ", format_value_iodata(value)]
      end)

    ["%{", Enum.intersperse(entries, ", "), "}"]
  end

  defp format_map_key(key) when is_atom(key), do: Atom.to_string(key)
  defp format_map_key(key) when is_binary(key), do: inspect(key)

  defp format_map_key(key) when is_list(key) do
    if charlist?(key), do: inspect(to_string(key)), else: inspect(key)
  end

  defp format_map_key(key), do: inspect(key)

  defp format_field_name(key) when is_atom(key), do: Atom.to_string(key)
  defp format_field_name(key), do: format_map_key(key)

  defp map_key_sort_key(key) when is_atom(key), do: Atom.to_string(key)
  defp map_key_sort_key(key), do: inspect(key)

  defp operator_field?(key) when is_atom(key) do
    Atom.to_string(key) |> String.starts_with?("__op_")
  end

  defp operator_field?(_key), do: false

  defp charlist?(list) do
    list != [] and Enum.all?(list, &is_integer/1)
  end

  defp optional_map?(map) do
    Map.get(map, :kind) == 1 and Map.has_key?(map, :tag)
  end

  defp result_map?(map) do
    Map.get(map, :kind) == 2 and Map.has_key?(map, :tag)
  end

  defp list_data(value) when is_map(value) do
    if list_map?(value), do: Map.get(value, :data), else: []
  end

  defp list_data(value) when is_list(value), do: value
  defp list_data(_value), do: []

  defp list_map?(value) do
    is_map(value) and Map.has_key?(value, :data) and Map.has_key?(value, :length) and
      Map.has_key?(value, :get) and not string_map?(value)
  end

  defp ensure_charlist(value) when is_binary(value), do: String.to_charlist(value)

  defp ensure_charlist(value) when is_map(value) do
    if string_map?(value), do: Map.get(value, :data), else: value
  end

  defp ensure_charlist(value), do: value

  defp string_map?(value) do
    is_map(value) and Map.has_key?(value, :data) and Map.has_key?(value, :length) and
      Map.has_key?(value, :chars)
  end

  @spec parse_args(list(), list(), list(), charlist(), map()) :: map()
  def parse_args(fields, field_types, field_annotations, type_label, args) do
    args_data = args.data
    field_strs = Enum.map(fields, &to_string/1)
    module = current_module()

    # Build field specs from annotations
    field_specs = build_field_specs(field_strs, field_types, field_annotations)

    # Check for --help / -h first
    if has_help_flag?(args_data) do
      error_struct = %{
        message: stdlib_string_new("--help"),
        missing: wrap_as_beamlang_list([], module),
        __beamlang_type__: ~c"ArgsError"
      }

      %{tag: 0, value: error_struct}
    else
      # Parse the args with annotation support
      case parse_args_with_annotations(args_data, field_specs, type_label, module) do
        {:ok, result} -> %{tag: 1, value: result}
        {:error, error_struct} -> %{tag: 0, value: error_struct}
      end
    end
  end

  @spec args_usage(list(), list(), list(), charlist(), map()) :: map()
  def args_usage(fields, field_types, field_annotations, _type_label, program) do
    field_strs = Enum.map(fields, &to_string/1)
    field_specs = build_field_specs(field_strs, field_types, field_annotations)
    program_str = string_data(program) |> to_string()
    usage_text = generate_usage(program_str, field_specs)
    stdlib_string_new(usage_text)
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
        required:
          if anns == nil or anns == [] do
            true
          else
            Map.get(ann_map, "required", false)
          end,
        default: Map.get(ann_map, "default"),
        description: Map.get(ann_map, "description"),
        short: Map.get(ann_map, "short"),
        long: Map.get(ann_map, "long"),
        flag: Map.get(ann_map, "flag", false),
        positional:
          !Map.has_key?(ann_map, "short") and !Map.has_key?(ann_map, "long") and
            !Map.get(ann_map, "flag", false),
        # Track whether this field has any annotations (for backwards compatibility)
        has_annotations: anns != nil and anns != []
      }
    end)
  end

  # Check if --help or -h is present in the raw args
  defp has_help_flag?(args_data) do
    Enum.any?(args_data, fn arg ->
      s = string_value(arg)
      s == "--help" or s == "-h"
    end)
  end

  # Generate formatted usage/help text from field specs
  defp generate_usage(program, field_specs) do
    {named_specs, positional_specs} =
      Enum.split_with(field_specs, fn spec -> !spec.positional end)

    # Build the synopsis line
    positional_parts =
      Enum.map(positional_specs, fn spec ->
        label = String.upcase(spec.name)
        if spec.required, do: "<#{label}>", else: "[#{label}]"
      end)

    options_part = if named_specs != [], do: " [OPTIONS]", else: ""

    positional_part =
      if positional_parts != [], do: " " <> Enum.join(positional_parts, " "), else: ""

    synopsis = "Usage: #{program}#{options_part}#{positional_part}"

    # Build positional arguments section
    positional_section =
      if positional_specs != [] do
        lines =
          Enum.map(positional_specs, fn spec ->
            label = String.upcase(spec.name)
            desc = spec.description || ""
            type_str = format_type_label(spec.type)
            required_str = if spec.required, do: " (required)", else: ""

            default_str =
              if spec.default != nil and spec.default != "",
                do: " [default: #{spec.default}]",
                else: ""

            "  #{String.pad_trailing(label, 20)} #{type_str}#{required_str}#{default_str}  #{desc}"
          end)

        "\n\nArguments:\n" <> Enum.join(lines, "\n")
      else
        ""
      end

    # Build options section
    options_section =
      if named_specs != [] do
        lines =
          Enum.map(named_specs, fn spec ->
            short_str = if spec.short, do: "-#{spec.short}", else: nil
            long_str = if spec.long, do: "--#{spec.long}", else: "--#{spec.name}"
            flag_parts = [short_str, long_str] |> Enum.reject(&is_nil/1) |> Enum.join(", ")

            value_hint =
              if spec.flag do
                ""
              else
                type_str = format_type_label(spec.type)
                " <#{type_str}>"
              end

            left = "  #{flag_parts}#{value_hint}"
            desc = spec.description || ""

            default_str =
              if spec.default != nil and spec.default != "",
                do: " [default: #{spec.default}]",
                else: ""

            "#{String.pad_trailing(left, 28)} #{desc}#{default_str}"
          end)

        help_line = "  #{String.pad_trailing("-h, --help", 28)} Show this help message"
        "\n\nOptions:\n" <> Enum.join(lines ++ [help_line], "\n")
      else
        "\n\nOptions:\n  #{String.pad_trailing("-h, --help", 28)} Show this help message"
      end

    String.trim_trailing(synopsis <> positional_section <> options_section)
  end

  defp format_type_label(:String), do: "STRING"
  defp format_type_label({:named, "String"}), do: "STRING"
  defp format_type_label(:number), do: "NUMBER"
  defp format_type_label({:named, "number"}), do: "NUMBER"
  defp format_type_label(:bool), do: "BOOL"
  defp format_type_label({:named, "bool"}), do: "BOOL"
  defp format_type_label(:char), do: "CHAR"
  defp format_type_label({:named, "char"}), do: "CHAR"
  defp format_type_label(_), do: "VALUE"

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

      _, acc ->
        acc
    end)
  end

  defp annotation_value(v) when is_list(v), do: to_string(v)
  defp annotation_value(v), do: v

  # Parse args with annotation support
  defp parse_args_with_annotations(args_data, field_specs, type_label, module) do
    # Separate named/flag args from positional args
    {named_specs, positional_specs} =
      Enum.split_with(field_specs, fn spec -> !spec.positional end)

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

      {:error, _} = err ->
        err
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
      # "--" separator: everything after is positional
      arg_str == "--" ->
        {Enum.reverse(positional_acc) ++ rest, acc}

      # Skip --help/-h (handled earlier in parse_args)
      arg_str == "--help" or arg_str == "-h" ->
        do_parse_named_args(rest, specs, acc, positional_acc)

      # Long form: --name=value or --name value
      String.starts_with?(arg_str, "--") ->
        case parse_long_arg(arg_str, rest, specs) do
          {:ok, field_name, value, remaining} ->
            do_parse_named_args(remaining, specs, Map.put(acc, field_name, value), positional_acc)

          :not_matched ->
            # Unrecognized flag, treat as positional
            do_parse_named_args(rest, specs, acc, [arg | positional_acc])
        end

      # Short form: -n=value, -n value, or combined flags -inv
      String.starts_with?(arg_str, "-") and String.length(arg_str) >= 2 ->
        case parse_short_arg(arg_str, rest, specs) do
          {:ok, field_name, value, remaining} ->
            do_parse_named_args(remaining, specs, Map.put(acc, field_name, value), positional_acc)

          :not_matched ->
            # Try combined short flags (e.g., -inv = -i -n -v)
            case parse_combined_short_flags(arg_str, specs) do
              {:ok, flag_values} ->
                merged_acc = Map.merge(acc, flag_values)
                do_parse_named_args(rest, specs, merged_acc, positional_acc)

              :not_matched ->
                # Unrecognized flag, treat as positional
                do_parse_named_args(rest, specs, acc, [arg | positional_acc])
            end
        end

      true ->
        # Positional argument, continue collecting
        do_parse_named_args(rest, specs, acc, [arg | positional_acc])
    end
  end

  # Parse combined short flags like -inv -> -i -n -v
  # Only valid if ALL characters match flag-type short options
  defp parse_combined_short_flags(arg_str, specs) do
    without_prefix = String.slice(arg_str, 1, String.length(arg_str) - 1)
    chars = String.graphemes(without_prefix)

    # Only try combined flags if more than 1 character
    if length(chars) <= 1 do
      :not_matched
    else
      results =
        Enum.reduce_while(chars, {:ok, %{}}, fn char, {:ok, acc} ->
          case find_spec_by_short(char, specs) do
            nil ->
              {:halt, :not_matched}

            spec ->
              if spec.flag do
                {:cont, {:ok, Map.put(acc, spec.name, true)}}
              else
                # Non-flag short option in combined form — not allowed
                {:halt, :not_matched}
              end
          end
        end)

      case results do
        {:ok, values} -> {:ok, values}
        :not_matched -> :not_matched
      end
    end
  end

  defp parse_long_arg(arg_str, rest, specs) do
    # Remove "--" prefix
    without_prefix = String.slice(arg_str, 2, String.length(arg_str) - 2)

    # Check for --name=value form
    case String.split(without_prefix, "=", parts: 2) do
      [name, value] ->
        case find_spec_by_long(name, specs) do
          nil ->
            :not_matched

          spec ->
            parsed_value = if spec.flag, do: true, else: value
            {:ok, spec.name, parsed_value, rest}
        end

      [name] ->
        case find_spec_by_long(name, specs) do
          nil ->
            :not_matched

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
          nil ->
            :not_matched

          spec ->
            parsed_value = if spec.flag, do: true, else: value
            {:ok, spec.name, parsed_value, rest}
        end

      [name] when byte_size(name) == 1 ->
        case find_spec_by_short(name, specs) do
          nil ->
            :not_matched

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
    result =
      Enum.reduce_while(field_specs, %{}, fn spec, acc ->
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
      {:error, _} = err ->
        err

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
