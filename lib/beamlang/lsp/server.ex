defmodule BeamLang.LSP.Server do
  @moduledoc false

  alias BeamLang.LSP.Protocol

  @type document :: %{
          uri: binary(),
          path: binary(),
          text: binary(),
          tokens: [BeamLang.Token.t()],
          ast: BeamLang.AST.t() | nil,
          index: map()
        }

  @spec start() :: :ok
  def start do
    loop(%{documents: %{}})
  end

  defp loop(state) do
    case Protocol.read_message() do
      :eof ->
        :ok

      {:ok, message} ->
        state
        |> handle_message(message)
        |> loop()
    end
  end

  defp handle_message(state, %{"method" => "initialize", "id" => id}) do
    Protocol.send_response(id, %{
      "capabilities" => %{
        "textDocumentSync" => %{"openClose" => true, "change" => 1},
        "hoverProvider" => true,
        "definitionProvider" => true,
        "completionProvider" => %{"resolveProvider" => false}
      }
    })

    state
  end

  defp handle_message(state, %{"method" => "shutdown", "id" => id}) do
    Protocol.send_response(id, nil)
    state
  end

  defp handle_message(state, %{"method" => "exit"}) do
    System.halt(0)
    state
  end

  defp handle_message(state, %{"method" => "textDocument/didOpen", "params" => params}) do
    %{"textDocument" => %{"uri" => uri, "text" => text, "version" => _version}} = params
    {doc, diagnostics} = build_document(uri, text)
    publish_diagnostics(doc, diagnostics)
    put_in(state[:documents][uri], doc)
  end

  defp handle_message(state, %{"method" => "textDocument/didChange", "params" => params}) do
    %{"textDocument" => %{"uri" => uri}, "contentChanges" => changes} = params
    text = latest_change_text(changes)
    {doc, diagnostics} = build_document(uri, text)
    publish_diagnostics(doc, diagnostics)
    put_in(state[:documents][uri], doc)
  end

  defp handle_message(state, %{"method" => "textDocument/didClose", "params" => params}) do
    %{"textDocument" => %{"uri" => uri}} = params
    Protocol.send_notification("textDocument/publishDiagnostics", %{"uri" => uri, "diagnostics" => []})
    update_in(state[:documents], &Map.delete(&1, uri))
  end

  defp handle_message(state, %{"method" => "textDocument/hover", "id" => id, "params" => params}) do
    hover =
      with %{"textDocument" => %{"uri" => uri}, "position" => position} <- params,
           doc when not is_nil(doc) <- Map.get(state.documents, uri) do
        hover_for(doc, position)
      else
        _ -> nil
      end

    Protocol.send_response(id, hover)
    state
  end

  defp handle_message(state, %{"method" => "textDocument/definition", "id" => id, "params" => params}) do
    definition =
      with %{"textDocument" => %{"uri" => uri}, "position" => position} <- params,
           doc when not is_nil(doc) <- Map.get(state.documents, uri) do
        definition_for(doc, position)
      else
        _ -> []
      end

    Protocol.send_response(id, definition)
    state
  end

  defp handle_message(state, %{"method" => "textDocument/completion", "id" => id, "params" => params}) do
    completion =
      with %{"textDocument" => %{"uri" => uri}} <- params,
           doc when not is_nil(doc) <- Map.get(state.documents, uri) do
        completion_for(doc)
      else
        _ -> %{"isIncomplete" => false, "items" => []}
      end

    Protocol.send_response(id, completion)
    state
  end

  defp handle_message(state, _message) do
    state
  end

  defp build_document(uri, text) do
    path = uri_to_path(uri)

    {tokens, ast, diagnostics} =
      case BeamLang.analyze_source(text, path) do
        {:ok, %{tokens: tokens, ast: ast, errors: errors}} ->
          {tokens, ast, errors}

        {:error, errors} when is_list(errors) ->
          tokens =
            case BeamLang.Lexer.tokenize(text, path) do
              {:ok, tokens} -> tokens
              _ -> []
            end

          ast =
            case BeamLang.Parser.parse(tokens) do
              {:ok, ast} -> ast
              _ -> nil
            end

          {tokens, ast, errors}
      end

    index = build_index(ast)

    doc = %{
      uri: uri,
      path: path,
      text: text,
      tokens: tokens,
      ast: ast,
      index: index
    }

    {doc, diagnostics}
  end

  defp publish_diagnostics(doc, errors) do
    diagnostics =
      errors
      |> Enum.filter(fn err -> err.span.file_id == doc.path or err.span.file_id == "<source>" end)
      |> Enum.map(&diagnostic_for(doc.text, &1))

    Protocol.send_notification("textDocument/publishDiagnostics", %{
      "uri" => doc.uri,
      "diagnostics" => diagnostics
    })
  end

  defp diagnostic_for(source, %BeamLang.Error{} = error) do
    %{
      "range" => range_for_span(source, error.span),
      "severity" => 1,
      "source" => "BeamLang",
      "message" => error.message
    }
  end

  defp hover_for(doc, %{"line" => line, "character" => character}) do
    offset = position_to_offset(doc.text, line, character)

    with %BeamLang.Token{type: :identifier, value: name} <- token_at(doc.tokens, offset),
         {:ok, info} <- lookup_hover(doc.index, name) do
      %{
        "contents" => %{
          "kind" => "plaintext",
          "value" => info
        }
      }
    else
      _ -> nil
    end
  end

  defp definition_for(doc, %{"line" => line, "character" => character}) do
    offset = position_to_offset(doc.text, line, character)

    with %BeamLang.Token{type: :identifier, value: name} <- token_at(doc.tokens, offset),
         {:ok, %{span: span, path: path}} <- lookup_definition(doc.index, name, doc.path) do
      [
        %{
          "uri" => path_to_uri(path),
          "range" => range_for_span(doc.text, span)
        }
      ]
    else
      _ -> []
    end
  end

  defp completion_for(doc) do
    %{"isIncomplete" => false, "items" => completion_items(doc.index)}
  end

  defp completion_items(index) do
    functions =
      index
      |> Map.get(:functions, %{})
      |> Enum.map(fn {name, _} -> %{"label" => name, "kind" => 3} end)

    types =
      index
      |> Map.get(:types, %{})
      |> Enum.map(fn {name, _} -> %{"label" => name, "kind" => 7} end)

    errors =
      index
      |> Map.get(:errors, %{})
      |> Enum.map(fn {name, _} -> %{"label" => name, "kind" => 7} end)

    functions ++ types ++ errors
  end

  defp lookup_hover(index, name) do
    case Map.get(index[:functions] || %{}, name) do
      nil ->
        case Map.get(index[:types] || %{}, name) do
          nil ->
            case Map.get(index[:errors] || %{}, name) do
              nil -> :error
              info -> {:ok, format_type_info("error", name, info.fields)}
            end

          info ->
            {:ok, format_type_info("type", name, info.fields)}
        end

      info ->
        {:ok, format_function_info(name, info.params, info.return_type)}
    end
  end

  defp lookup_definition(index, name, path) do
    case Map.get(index[:functions] || %{}, name) do
      nil ->
        case Map.get(index[:types] || %{}, name) do
          nil ->
            case Map.get(index[:errors] || %{}, name) do
              nil -> :error
              info -> {:ok, %{span: info.span, path: path}}
            end

          info ->
            {:ok, %{span: info.span, path: path}}
        end

      info ->
        {:ok, %{span: info.span, path: path}}
    end
  end

  defp build_index(nil), do: %{}

  defp build_index({:program, %{functions: functions, types: types, errors: errors}}) do
    %{
      functions: index_functions(functions),
      types: index_types(types),
      errors: index_errors(errors)
    }
  end

  defp build_index({:program, %{functions: functions, types: types} = program}) do
    errors = Map.get(program, :errors, [])

    %{
      functions: index_functions(functions),
      types: index_types(types),
      errors: index_errors(errors)
    }
  end

  defp index_functions(functions) do
    Enum.reduce(functions, %{}, fn {:function, %{name: name, params: params, return_type: return_type, span: span}}, acc ->
      Map.put(acc, name, %{params: params, return_type: return_type, span: span})
    end)
  end

  defp index_types(types) do
    Enum.reduce(types, %{}, fn {:type_def, %{name: name, fields: fields, span: span}}, acc ->
      Map.put(acc, name, %{fields: fields, span: span})
    end)
  end

  defp index_errors(errors) do
    Enum.reduce(errors, %{}, fn {:error_def, %{name: name, fields: fields, span: span}}, acc ->
      Map.put(acc, name, %{fields: fields, span: span})
    end)
  end

  defp format_function_info(name, params, return_type) do
    param_list =
      params
      |> Enum.map(fn %{name: param_name, type: type} ->
        "#{param_name}: #{format_type(type)}"
      end)
      |> Enum.join(", ")

    "fn #{name}(#{param_list}) -> #{format_type(return_type)}"
  end

  defp format_type_info(kind, name, fields) do
    field_list =
      fields
      |> Enum.map(fn %{name: field_name, type: type} ->
        "#{field_name}: #{format_type(type)}"
      end)
      |> Enum.join(", ")

    "#{kind} #{name} { #{field_list} }"
  end

  defp format_type({:generic, base, args}) do
    "#{format_type(base)}<#{Enum.map_join(args, ", ", &format_type/1)}>"
  end

  defp format_type({:type_var, name}) when is_binary(name), do: name
  defp format_type({:named, name}) when is_binary(name), do: name
  defp format_type({:optional, inner}), do: "#{format_type(inner)}?"
  defp format_type({:result, ok_type, err_type}), do: "#{format_type(ok_type)}!#{format_type(err_type)}"
  defp format_type({:fn, params, return_type}) do
    "fn(#{Enum.map_join(params, ", ", &format_type/1)}) -> #{format_type(return_type)}"
  end

  defp format_type(type) when is_atom(type), do: Atom.to_string(type)

  defp token_at(tokens, offset) do
    Enum.find(tokens, fn %BeamLang.Token{span: span} ->
      span.start <= offset and offset < span.end
    end)
  end

  defp range_for_span(source, %BeamLang.Span{start: start_offset, end: end_offset}) do
    %{
      "start" => offset_to_position(source, start_offset),
      "end" => offset_to_position(source, end_offset)
    }
  end

  defp offset_to_position(source, offset) do
    lines = String.split(source, "\n", trim: false)

    {line_idx, line_start} =
      Enum.reduce_while(lines, {0, 0}, fn line, {idx, start_offset} ->
        line_len = byte_size(String.trim_trailing(line, "\r"))

        if offset <= start_offset + line_len do
          {:halt, {idx, start_offset}}
        else
          {:cont, {idx + 1, start_offset + line_len + 1}}
        end
      end)

    line_text = Enum.at(lines, line_idx, "") |> String.trim_trailing("\r")
    byte_col = max(offset - line_start, 0)
    char_col = String.length(binary_slice(line_text, 0, min(byte_col, byte_size(line_text))))

    %{"line" => line_idx, "character" => char_col}
  end

  defp position_to_offset(source, line, character) do
    lines = String.split(source, "\n", trim: false)
    line_text = Enum.at(lines, line, "") |> String.trim_trailing("\r")
    prefix = String.slice(line_text, 0, character)

    line_start =
      lines
      |> Enum.take(line)
      |> Enum.reduce(0, fn line_text, acc ->
        acc + byte_size(String.trim_trailing(line_text, "\r")) + 1
      end)

    line_start + byte_size(prefix)
  end

  defp latest_change_text([]), do: ""
  defp latest_change_text(changes) when is_list(changes) do
    changes
    |> List.last()
    |> Map.get("text", "")
  end

  defp uri_to_path("file://" <> rest) do
    URI.decode(rest)
  end

  defp uri_to_path(uri), do: uri

  defp path_to_uri(path) do
    if String.starts_with?(path, "file://") do
      path
    else
      "file://" <> URI.encode(path, &(&1 == ?/ or URI.char_unreserved?(&1)))
    end
  end
end
