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
        "completionProvider" => %{"resolveProvider" => false},
        "documentSymbolProvider" => true,
        "workspaceSymbolProvider" => true,
        "signatureHelpProvider" => %{"triggerCharacters" => ["(", ","]}
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
      with %{"textDocument" => %{"uri" => uri}, "position" => position} <- params,
           doc when not is_nil(doc) <- Map.get(state.documents, uri) do
        completion_for(doc, position)
      else
        _ -> %{"isIncomplete" => false, "items" => []}
      end

    Protocol.send_response(id, completion)
    state
  end

  defp handle_message(state, %{"method" => "textDocument/documentSymbol", "id" => id, "params" => params}) do
    symbols =
      with %{"textDocument" => %{"uri" => uri}} <- params,
           doc when not is_nil(doc) <- Map.get(state.documents, uri) do
        document_symbols_for(doc)
      else
        _ -> []
      end

    Protocol.send_response(id, symbols)
    state
  end

  defp handle_message(state, %{"method" => "workspace/symbol", "id" => id, "params" => params}) do
    %{"query" => query} = params
    symbols = workspace_symbols_for(state, query || "")
    Protocol.send_response(id, symbols)
    state
  end

  defp handle_message(state, %{"method" => "textDocument/signatureHelp", "id" => id, "params" => params}) do
    signature =
      with %{"textDocument" => %{"uri" => uri}, "position" => position} <- params,
           doc when not is_nil(doc) <- Map.get(state.documents, uri) do
        signature_help_for(doc, position)
      else
        _ -> nil
      end

    Protocol.send_response(id, signature)
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

    with %BeamLang.Token{type: :identifier, value: name} <- identifier_at(doc, offset),
         {:ok, info} <- lookup_hover(doc, name, offset) do
      %{
        "contents" => %{
          "kind" => "markdown",
          "value" => "```beamlang\n" <> info <> "\n```"
        }
      }
    else
      _ -> nil
    end
  end

  defp definition_for(doc, %{"line" => line, "character" => character}) do
    offset = position_to_offset(doc.text, line, character)

    with %BeamLang.Token{type: :identifier, value: name} <- identifier_at(doc, offset),
         {:ok, %{span: span, path: path}} <- lookup_definition(doc, name, offset) do
      [
        %{
          "uri" => path_to_uri(path),
          "range" => range_for_span_in_doc(doc, span)
        }
      ]
    else
      _ -> []
    end
  end

  defp completion_for(doc, %{"line" => line, "character" => character}) do
    offset = position_to_offset(doc.text, line, character)
    context = completion_context(doc, offset)
    %{"isIncomplete" => false, "items" => completion_items(doc, context, offset)}
  end

  defp document_symbols_for(doc) do
    functions =
      doc.index
      |> Map.get(:functions, %{})
      |> Enum.filter(fn {_name, info} -> span_in_doc?(doc, info.span) end)
      |> Enum.map(fn {name, info} ->
        document_symbol(name, 12, doc.text, info.span)
      end)

    types =
      doc.index
      |> Map.get(:types, %{})
      |> Enum.filter(fn {_name, info} -> span_in_doc?(doc, info.span) end)
      |> Enum.map(fn {name, info} ->
        document_symbol(name, 23, doc.text, info.span)
      end)

    errors =
      doc.index
      |> Map.get(:errors, %{})
      |> Enum.filter(fn {_name, info} -> span_in_doc?(doc, info.span) end)
      |> Enum.map(fn {name, info} ->
        document_symbol(name, 23, doc.text, info.span)
      end)

    methods =
      doc.index
      |> Map.get(:methods, %{})
      |> Enum.flat_map(fn {name, infos} ->
        infos
        |> Enum.filter(fn info -> span_in_doc?(doc, info.span) end)
        |> Enum.map(fn info ->
          document_symbol("#{info.type_name}::#{name}", 6, doc.text, info.span)
        end)
      end)

    functions ++ types ++ errors ++ methods
  end

  defp workspace_symbols_for(state, query) do
    state.documents
    |> Enum.flat_map(fn {_uri, doc} ->
      doc.index
      |> symbol_matches(query)
      |> Enum.map(fn {name, kind, span} ->
        if span_in_doc?(doc, span) do
        %{
          "name" => name,
          "kind" => kind,
          "location" => %{
            "uri" => doc.uri,
            "range" => range_for_span_in_doc(doc, span)
          }
        }
        else
          nil
        end
      end)
      |> Enum.reject(&is_nil/1)
    end)
  end

  defp signature_help_for(doc, %{"line" => line, "character" => character}) do
    offset = position_to_offset(doc.text, line, character)
    case infer_call_name(doc.tokens, offset) do
      nil ->
        nil

      name ->
        case Map.get(doc.index[:functions] || %{}, name) do
          nil ->
            case Map.get(doc.index[:methods] || %{}, name) do
              nil ->
                nil

              [info | _] ->
                label = format_method_info(info.type_name, name, info.params, info.return_type)
                %{
                  "signatures" => [
                    %{
                      "label" => label,
                      "parameters" =>
                        Enum.map(info.params, fn param ->
                          %{"label" => "#{param.name}: #{format_type(param.type)}"}
                        end)
                    }
                  ],
                  "activeSignature" => 0,
                  "activeParameter" => 0
                }
            end

          info ->
            label = format_function_info(name, info.params, info.return_type)
            %{
              "signatures" => [
                %{
                  "label" => label,
                  "parameters" =>
                    Enum.map(info.params, fn %{name: param_name, type: type} ->
                      %{"label" => "#{param_name}: #{format_type(type)}"}
                    end)
                }
              ],
              "activeSignature" => 0,
              "activeParameter" => 0
            }
        end
    end
  end

  defp completion_items(doc, context, offset) do
    functions =
      doc.index
      |> Map.get(:functions, %{})
      |> Enum.filter(fn {name, info} -> public_function?(doc, name, info) end)
      |> Enum.map(fn {name, _} -> %{"label" => name, "kind" => 3} end)

    types =
      doc.index
      |> Map.get(:types, %{})
      |> Enum.map(fn {name, _} -> %{"label" => name, "kind" => 7} end)

    errors =
      doc.index
      |> Map.get(:errors, %{})
      |> Enum.map(fn {name, _} -> %{"label" => name, "kind" => 7} end)

    methods =
      doc.index
      |> Map.get(:methods, %{})
      |> Enum.map(fn {name, _} -> %{"label" => name, "kind" => 6} end)

    locals =
      doc.index
      |> Map.get(:locals, [])
      |> Enum.filter(fn local ->
        local.func_span.file_id == doc.path and span_contains?(local.scope_span, offset)
      end)
      |> Enum.map(fn local -> %{"label" => local.name, "kind" => 6} end)

    base =
      case context do
        :methods ->
          locals ++ methods

        :statement ->
          locals ++ functions ++ statement_keywords()

        :expression ->
          locals ++ functions ++ methods ++ types ++ errors ++ statement_keywords()

        _ ->
          locals ++ functions ++ methods ++ types ++ errors ++ statement_keywords()
      end

    base
    |> Enum.uniq_by(& &1["label"])
  end

  defp lookup_hover(doc, name, offset) do
    case lookup_local(doc, name, offset) do
      {:ok, info} ->
        {:ok, format_local_info(info)}

      :error ->
        case Map.get(doc.index[:functions] || %{}, name) do
          nil ->
            case Map.get(doc.index[:types] || %{}, name) do
              nil ->
                case Map.get(doc.index[:errors] || %{}, name) do
                  nil ->
                    case Map.get(doc.index[:methods] || %{}, name) do
                      nil -> :error
                      infos -> {:ok, format_methods_info(name, infos)}
                    end

                  info -> {:ok, format_type_info("error", name, info.fields)}
                end

              info ->
                {:ok, format_type_info("type", name, info.fields)}
            end

          info ->
            {:ok, format_function_info(name, info.params, info.return_type)}
        end
    end
  end

  defp lookup_definition(doc, name, offset) do
    case lookup_local(doc, name, offset) do
      {:ok, info} ->
        {:ok, %{span: info.span, path: doc.path}}

      :error ->
        case Map.get(doc.index[:functions] || %{}, name) do
          nil ->
            case Map.get(doc.index[:types] || %{}, name) do
              nil ->
                case Map.get(doc.index[:errors] || %{}, name) do
                  nil ->
                    case Map.get(doc.index[:methods] || %{}, name) do
                      nil -> :error
                      [info | _] -> {:ok, %{span: info.span, path: doc.path}}
                    end

                  info -> {:ok, %{span: info.span, path: doc.path}}
                end

              info ->
                {:ok, %{span: info.span, path: doc.path}}
            end

          info ->
            {:ok, %{span: info.span, path: doc.path}}
        end
    end
  end

  defp symbol_matches(index, query) do
    query = String.downcase(query)

    functions =
      index
      |> Map.get(:functions, %{})
      |> Enum.filter(fn {name, _} -> match_query?(name, query) end)
      |> Enum.map(fn {name, info} -> {name, 12, info.span} end)

    types =
      index
      |> Map.get(:types, %{})
      |> Enum.filter(fn {name, _} -> match_query?(name, query) end)
      |> Enum.map(fn {name, info} -> {name, 23, info.span} end)

    errors =
      index
      |> Map.get(:errors, %{})
      |> Enum.filter(fn {name, _} -> match_query?(name, query) end)
      |> Enum.map(fn {name, info} -> {name, 23, info.span} end)

    methods =
      index
      |> Map.get(:methods, %{})
      |> Enum.flat_map(fn {name, infos} ->
        Enum.filter(infos, fn info -> match_query?("#{info.type_name}::#{name}", query) end)
        |> Enum.map(fn info -> {"#{info.type_name}::#{name}", 6, info.span} end)
      end)

    functions ++ types ++ errors ++ methods
  end

  defp match_query?(_name, ""), do: true
  defp match_query?(name, query), do: String.contains?(String.downcase(name), query)

  defp build_index(nil), do: %{}

  defp build_index({:program, %{functions: functions, types: types, errors: errors}}) do
    %{
      functions: index_functions(functions),
      types: index_types(types),
      errors: index_errors(errors),
      methods: index_methods(types),
      locals: index_locals(functions)
    }
  end

  defp build_index({:program, %{functions: functions, types: types} = program}) do
    errors = Map.get(program, :errors, [])

    %{
      functions: index_functions(functions),
      types: index_types(types),
      errors: index_errors(errors),
      methods: index_methods(types),
      locals: index_locals(functions)
    }
  end

  defp index_functions(functions) do
    Enum.reduce(functions, %{}, fn {:function, %{name: name, params: params, return_type: return_type, span: span} = func}, acc ->
      info = %{
        params: params,
        return_type: return_type,
        span: span,
        exported: Map.get(func, :exported, false),
        internal: Map.get(func, :internal, false),
        external: Map.get(func, :external)
      }

      Map.put(acc, name, info)
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

  defp index_methods(types) do
    Enum.reduce(types, %{}, fn {:type_def, %{name: type_name, fields: fields}}, acc ->
      Enum.reduce(fields, acc, fn field, acc2 ->
        case field do
          %{name: name, type: {:fn, params, return_type}, span: span} ->
            info = %{type_name: type_name, params: params, return_type: return_type, span: span}
            Map.update(acc2, name, [info], fn infos -> [info | infos] end)

          _ ->
            acc2
        end
      end)
    end)
  end

  defp index_locals(functions) do
    Enum.flat_map(functions, fn {:function, %{params: params, body: body, span: func_span}} ->
      scope_span =
        case body do
          {:block, %{span: span}} -> span
          _ -> func_span
        end

      param_entries =
        Enum.map(params, fn %{name: name, type: type, span: span} ->
          %{name: name, type: type, span: span, func_span: func_span, scope_span: scope_span, kind: :param}
        end)

      param_entries ++ collect_lets(body, func_span)
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

  defp format_method_info(type_name, name, params, return_type) do
    param_list =
      params
      |> Enum.map(fn %{name: param_name, type: type} ->
        "#{param_name}: #{format_type(type)}"
      end)
      |> Enum.join(", ")

    "fn #{type_name}::#{name}(#{param_list}) -> #{format_type(return_type)}"
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

  defp format_local_info(%{name: name, type: nil, kind: kind}) do
    "#{kind_label(kind)} #{name}"
  end

  defp format_local_info(%{name: name, type: type, kind: kind}) do
    "#{kind_label(kind)} #{name}: #{format_type(type)}"
  end

  defp format_methods_info(name, infos) do
    infos
    |> Enum.map(fn info -> format_method_info(info.type_name, name, info.params, info.return_type) end)
    |> Enum.join("\n")
  end

  defp kind_label(:param), do: "param"
  defp kind_label(:let), do: "let"
  defp kind_label(:for), do: "for"
  defp kind_label(_), do: "local"

  defp public_function?(doc, name, info) do
    cond do
      info.internal -> false
      info.span.file_id == doc.path -> true
      info.span.file_id == "<source>" -> true
      info.exported -> true
      is_map(info.external) and not String.ends_with?(name, "_data") -> true
      true -> false
    end
  end

  defp statement_keywords do
    [
      %{"label" => "let", "kind" => 14},
      %{"label" => "return", "kind" => 14},
      %{"label" => "if", "kind" => 14},
      %{"label" => "match", "kind" => 14},
      %{"label" => "while", "kind" => 14},
      %{"label" => "loop", "kind" => 14},
      %{"label" => "for", "kind" => 14},
      %{"label" => "guard", "kind" => 14}
    ]
  end

  defp document_symbol(name, kind, source, span) do
    %{
      "name" => name,
      "kind" => kind,
      "range" => range_for_span(source, span),
      "selectionRange" => range_for_span(source, span)
    }
  end

  defp token_at(tokens, offset) do
    Enum.find(tokens, fn %BeamLang.Token{span: span} ->
      span.start <= offset and offset < span.end
    end)
  end

  defp identifier_at(doc, offset) do
    case token_at(doc.tokens, offset) do
      %BeamLang.Token{type: :identifier} = token ->
        token

      _ ->
        case token_at(doc.tokens, max(offset - 1, 0)) do
          %BeamLang.Token{type: :identifier} = token ->
            token

          _ ->
            {line_start, line_end} = line_bounds(doc.text, offset)

            doc.tokens
            |> Enum.filter(fn %BeamLang.Token{type: type, span: span} ->
              type == :identifier and span.start >= line_start and span.start <= line_end
            end)
            |> Enum.min_by(fn %BeamLang.Token{span: span} -> abs(span.start - offset) end, fn -> nil end)
        end
    end
  end

  defp infer_call_name(tokens, offset) do
    tokens
    |> Enum.filter(fn %BeamLang.Token{span: span} -> span.start <= offset end)
    |> Enum.reverse()
    |> find_call_name()
  end

  defp find_call_name([]), do: nil

  defp find_call_name([%BeamLang.Token{type: :identifier, value: name}, %BeamLang.Token{type: :lparen} | _]) do
    name
  end

  defp find_call_name([_ | rest]), do: find_call_name(rest)

  defp completion_context(doc, offset) do
    case previous_token(doc.tokens, offset) do
      nil -> :statement
      %BeamLang.Token{type: :arrow} -> :methods
      %BeamLang.Token{type: :lbrace} -> :statement
      %BeamLang.Token{type: :semicolon} -> :statement
      _ -> :expression
    end
  end

  defp previous_token(tokens, offset) do
    tokens
    |> Enum.filter(fn %BeamLang.Token{span: span} -> span.start < offset end)
    |> Enum.max_by(fn %BeamLang.Token{span: span} -> span.start end, fn -> nil end)
  end

  defp lookup_local(doc, name, offset) do
    locals = Map.get(doc.index, :locals, [])

    locals
    |> Enum.filter(fn local ->
      local.name == name and
        local.func_span.file_id == doc.path and
        span_contains?(local.scope_span, offset) and
        offset >= local.func_span.start and offset <= local.func_span.end and
        local.span.start <= offset
    end)
    |> Enum.sort_by(fn local -> local.span.start end, :desc)
    |> List.first()
    |> case do
      nil -> :error
      local -> {:ok, local}
    end
  end

  defp collect_lets(nil, _func_span), do: []

  defp collect_lets({:block, %{stmts: stmts, span: block_span}}, func_span) do
    Enum.flat_map(stmts, &collect_from_stmt(&1, func_span, block_span))
  end

  defp collect_from_stmt({:let, %{name: name, type: type, expr: expr, span: span}}, func_span, scope_span) do
    inferred = type || infer_expr_type(expr)
    [%{name: name, type: inferred, span: span, func_span: func_span, scope_span: scope_span, kind: :let}] ++
      collect_from_expr(expr, func_span, scope_span)
  end

  defp collect_from_stmt({:for, %{name: name, body: body, span: span}}, func_span, scope_span) do
    [%{name: name, type: nil, span: span, func_span: func_span, scope_span: scope_span, kind: :for}] ++
      collect_lets(body, func_span)
  end

  defp collect_from_stmt({:if_stmt, %{then_block: then_block, else_branch: else_branch}}, func_span, _scope_span) do
    collect_lets(then_block, func_span) ++ collect_from_else(else_branch, func_span)
  end

  defp collect_from_stmt({:while, %{body: body}}, func_span, _scope_span), do: collect_lets(body, func_span)
  defp collect_from_stmt({:loop, %{body: body}}, func_span, _scope_span), do: collect_lets(body, func_span)
  defp collect_from_stmt({:guard, %{else_block: block}}, func_span, _scope_span), do: collect_lets(block, func_span)
  defp collect_from_stmt({:expr, %{expr: expr}}, func_span, scope_span), do: collect_from_expr(expr, func_span, scope_span)
  defp collect_from_stmt(_stmt, _func_span, _scope_span), do: []

  defp collect_from_else(nil, _func_span), do: []

  defp collect_from_else({:else_block, %{block: block}}, func_span) do
    collect_lets(block, func_span)
  end

  defp collect_from_else({:else_if, %{if: if_stmt}}, func_span) do
    collect_from_stmt(if_stmt, func_span, nil)
  end

  defp collect_from_expr(nil, _func_span, _scope_span), do: []
  defp collect_from_expr({:block_expr, %{block: block}}, func_span, _scope_span), do: collect_lets(block, func_span)
  defp collect_from_expr({:if_expr, %{then_block: then_block, else_branch: else_branch}}, func_span, _scope_span) do
    collect_lets(then_block, func_span) ++ collect_from_else(else_branch, func_span)
  end

  defp collect_from_expr({:match, %{cases: cases}}, func_span, _scope_span) do
    Enum.flat_map(cases, fn %{pattern: pattern, body: body, span: case_span} ->
      pattern_locals =
        pattern
        |> pattern_bindings()
        |> Enum.map(fn {name, span} ->
          %{name: name, type: nil, span: span, func_span: func_span, scope_span: case_span, kind: :let}
        end)

      pattern_locals ++ collect_from_expr(body, func_span, case_span)
    end)
  end

  defp collect_from_expr({:call, %{args: args}}, func_span, scope_span) do
    Enum.flat_map(args, &collect_from_expr(&1, func_span, scope_span))
  end

  defp collect_from_expr({:method_call, %{target: target, args: args}}, func_span, scope_span) do
    collect_from_expr(target, func_span, scope_span) ++ Enum.flat_map(args, &collect_from_expr(&1, func_span, scope_span))
  end

  defp collect_from_expr({:binary, %{left: left, right: right}}, func_span, scope_span) do
    collect_from_expr(left, func_span, scope_span) ++ collect_from_expr(right, func_span, scope_span)
  end

  defp collect_from_expr({:struct, %{fields: fields}}, func_span, scope_span) do
    Enum.flat_map(fields, fn %{expr: expr} -> collect_from_expr(expr, func_span, scope_span) end)
  end

  defp collect_from_expr(_expr, _func_span, _scope_span), do: []

  defp pattern_bindings({:pat_identifier, %{name: name, span: span}}), do: [{name, span}]
  defp pattern_bindings({:opt_some_pat, %{name: name, span: span}}), do: [{name, span}]
  defp pattern_bindings({:res_ok_pat, %{name: name, span: span}}), do: [{name, span}]
  defp pattern_bindings({:res_err_pat, %{name: name, span: span}}), do: [{name, span}]
  defp pattern_bindings({:struct_pattern, %{fields: fields}}) do
    Enum.flat_map(fields, fn %{pattern: pat} -> pattern_bindings(pat) end)
  end
  defp pattern_bindings(_), do: []

  defp infer_expr_type({:integer, _}), do: :number
  defp infer_expr_type({:float, _}), do: :number
  defp infer_expr_type({:string, _}), do: :String
  defp infer_expr_type({:char, _}), do: :char
  defp infer_expr_type({:bool, _}), do: :bool
  defp infer_expr_type({:struct, %{type: type}}) when not is_nil(type), do: type
  defp infer_expr_type({:opt_some, %{expr: expr}}) do
    case infer_expr_type(expr) do
      nil -> nil
      inner -> {:optional, inner}
    end
  end

  defp infer_expr_type({:opt_none, _}), do: {:optional, :any}

  defp infer_expr_type({:res_ok, %{expr: expr}}) do
    case infer_expr_type(expr) do
      nil -> nil
      ok_type -> {:result, ok_type, :any}
    end
  end

  defp infer_expr_type({:res_err, %{expr: expr}}) do
    case infer_expr_type(expr) do
      nil -> nil
      err_type -> {:result, :any, err_type}
    end
  end

  defp infer_expr_type(_expr), do: nil

  defp span_contains?(nil, _offset), do: false
  defp span_contains?(%BeamLang.Span{start: start_pos, end: end_pos}, offset) do
    offset >= start_pos and offset <= end_pos
  end

  defp range_for_span(source, %BeamLang.Span{start: start_offset, end: end_offset}) do
    %{
      "start" => offset_to_position(source, start_offset),
      "end" => offset_to_position(source, end_offset)
    }
  end

  defp range_for_span_in_doc(doc, %BeamLang.Span{file_id: file_id} = span) do
    source =
      cond do
        file_id == doc.path -> doc.text
        file_id == "<source>" -> doc.text
        true ->
          case File.read(file_id) do
            {:ok, contents} -> contents
            _ -> doc.text
          end
      end

    range_for_span(source, span)
  end

  defp span_in_doc?(doc, %BeamLang.Span{file_id: file_id}) do
    file_id == doc.path or file_id == "<source>"
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

  defp line_bounds(source, offset) do
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
    line_end = line_start + byte_size(line_text)
    {line_start, line_end}
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
