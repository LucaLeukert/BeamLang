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

    case hover_in_interpolated_string(doc, offset) do
      {:ok, info} ->
        %{
          "contents" => %{
            "kind" => "markdown",
            "value" => "```beamlang\n" <> info <> "\n```"
          }
        }

      :no_string ->
        with %BeamLang.Token{type: :identifier, value: name} <- identifier_at(doc, offset),
             {:ok, info} <- lookup_hover_with_field_access(doc, name, offset) do
          %{
            "contents" => %{
              "kind" => "markdown",
              "value" => "```beamlang\n" <> info <> "\n```"
            }
          }
        else
          _ -> nil
        end

      :no_interpolation ->
        nil
    end
  end

  # Try to get hover info, checking for field access pattern (target->field)
  defp lookup_hover_with_field_access(doc, field_name, offset) do
    # First check if this is a field access pattern: target->field
    case detect_field_access(doc, offset) do
      {:ok, target_name, _arrow_span, field_name_detected} when field_name_detected == field_name ->
        # This is a field access, get the target's type and field info
        case lookup_local(doc, target_name, offset) do
          {:ok, %{type: target_type}} when not is_nil(target_type) ->
            case lookup_field_type(doc, target_type, field_name) do
              {:ok, type_name, field_type} ->
                {:ok, "field #{field_name}: #{format_type(field_type)} (on #{type_name})"}

              :error ->
                # Field not found, fall back to regular lookup
                lookup_hover(doc, field_name, offset)
            end

          _ ->
            # Target type unknown, fall back to regular lookup
            lookup_hover(doc, field_name, offset)
        end

      _ ->
        # Not a field access, do regular lookup
        lookup_hover(doc, field_name, offset)
    end
  end

  # Detect if we're at a field access pattern: returns {:ok, target_name, arrow_span, field_name}
  # or :error if not a field access
  defp detect_field_access(doc, offset) do
    # Find the identifier at the current position (the field name)
    case identifier_at(doc, offset) do
      %BeamLang.Token{type: :identifier, value: field_name, span: field_span} ->
        # Look for arrow token before the field
        arrow_token = find_token_before(doc.tokens, field_span.start, :arrow)

        case arrow_token do
          %BeamLang.Token{type: :arrow, span: arrow_span} ->
            # Look for identifier before the arrow (the target)
            target_token = find_token_before(doc.tokens, arrow_span.start, :identifier)

            case target_token do
              %BeamLang.Token{type: :identifier, value: target_name} ->
                {:ok, target_name, arrow_span, field_name}

              _ ->
                :error
            end

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  # Find a token of a specific type immediately before the given offset
  defp find_token_before(tokens, offset, expected_type) do
    tokens
    |> Enum.filter(fn %BeamLang.Token{span: span} -> span.end <= offset end)
    |> Enum.max_by(fn %BeamLang.Token{span: span} -> span.end end, fn -> nil end)
    |> case do
      %BeamLang.Token{type: ^expected_type} = token -> token
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
            case instantiated_function_info(doc, name, offset, info) do
              nil -> {:ok, format_function_info(name, info.params, info.return_type)}
              signature -> {:ok, signature}
            end
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
    func_index = index_functions(functions)

    %{
      functions: func_index,
      types: index_types(types),
      errors: index_errors(errors),
      methods: index_methods(types),
      locals: index_locals(functions, func_index)
    }
  end

  defp build_index({:program, %{functions: functions, types: types} = program}) do
    errors = Map.get(program, :errors, [])
    func_index = index_functions(functions)

    %{
      functions: func_index,
      types: index_types(types),
      errors: index_errors(errors),
      methods: index_methods(types),
      locals: index_locals(functions, func_index)
    }
  end

  defp index_functions(functions) do
    Enum.reduce(functions, %{}, fn {:function, %{name: name, params: params, return_type: return_type, span: span, type_params: type_params} = func}, acc ->
      info = %{
        params: params,
        return_type: return_type,
        span: span,
        type_params: type_params,
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

  defp index_locals(functions, func_table) do
    Enum.flat_map(functions, fn {:function, %{params: params, body: body, span: func_span}} ->
      scope_span =
        case body do
          {:block, %{span: span}} -> span
          _ -> func_span
        end

      {env, param_entries} =
        Enum.reduce(params, {%{}, []}, fn %{name: name, type: type, span: span}, {env, acc} ->
          entry = %{name: name, type: type, span: span, func_span: func_span, scope_span: scope_span, kind: :param}
          {Map.put(env, name, type), [entry | acc]}
        end)

      {locals, _env} = collect_block_locals(body, func_span, func_table, env)
      Enum.reverse(param_entries) ++ locals
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

  defp instantiated_function_info(doc, name, offset, info) do
    case Map.get(info, :type_params, []) do
      [] -> nil
      _ ->
        case find_call_at_offset(doc.ast, name, offset) do
          nil -> nil
          {:call, args} ->
            subst = build_type_subst(doc, args, info.params, offset)
            if map_size(subst) == 0 do
              nil
            else
              param_list =
                info.params
                |> Enum.map(fn %{name: param_name, type: type} ->
                  "#{param_name}: #{format_type(substitute_type(type, subst))}"
                end)
                |> Enum.join(", ")

              return_type = substitute_type(info.return_type, subst)
              "fn #{name}(#{param_list}) -> #{format_type(return_type)}"
            end
        end
    end
  end

  defp build_type_subst(doc, args, params, offset) do
    Enum.zip(args, params)
    |> Enum.reduce(%{}, fn {arg, param}, acc ->
      case infer_expr_type_for_call(arg, doc, offset) do
        nil -> acc
        arg_type -> unify_types(param.type, arg_type, acc)
      end
    end)
  end

  defp unify_types({:type_var, name}, arg_type, subst) when is_binary(name) do
    Map.put_new(subst, name, arg_type)
  end

  defp unify_types({:generic, base_a, args_a}, {:generic, base_b, args_b}, subst) do
    subst =
      case {base_a, base_b} do
        {^base_a, ^base_b} -> subst
        _ -> subst
      end

    Enum.zip(args_a, args_b)
    |> Enum.reduce(subst, fn {a, b}, acc -> unify_types(a, b, acc) end)
  end

  defp unify_types({:optional, inner_a}, {:optional, inner_b}, subst), do: unify_types(inner_a, inner_b, subst)

  defp unify_types({:result, ok_a, err_a}, {:result, ok_b, err_b}, subst) do
    subst
    |> unify_types(ok_a, ok_b)
    |> unify_types(err_a, err_b)
  end

  defp unify_types({:fn, params_a, ret_a}, {:fn, params_b, ret_b}, subst) do
    subst =
      Enum.zip(params_a, params_b)
      |> Enum.reduce(subst, fn {a, b}, acc -> unify_types(a, b, acc) end)

    unify_types(ret_a, ret_b, subst)
  end

  defp unify_types(_param_type, _arg_type, subst), do: subst

  defp substitute_type({:type_var, name}, subst) when is_binary(name) do
    Map.get(subst, name, {:type_var, name})
  end

  defp substitute_type({:generic, base, args}, subst) do
    {:generic, substitute_type(base, subst), Enum.map(args, &substitute_type(&1, subst))}
  end

  defp substitute_type({:optional, inner}, subst), do: {:optional, substitute_type(inner, subst)}
  defp substitute_type({:result, ok_type, err_type}, subst),
    do: {:result, substitute_type(ok_type, subst), substitute_type(err_type, subst)}

  defp substitute_type({:fn, params, return_type}, subst) do
    {:fn, Enum.map(params, &substitute_type(&1, subst)), substitute_type(return_type, subst)}
  end

  defp substitute_type(type, _subst), do: type

  defp infer_expr_type_for_call({:identifier, %{name: name}}, doc, offset) do
    case lookup_local(doc, name, offset) do
      {:ok, info} -> info.type
      :error ->
        case Map.get(doc.index[:functions] || %{}, name) do
          nil -> nil
          info -> info.return_type
        end
    end
  end

  defp infer_expr_type_for_call({:call, %{name: name}}, doc, _offset) do
    case Map.get(doc.index[:functions] || %{}, name) do
      nil -> nil
      info -> info.return_type
    end
  end

  defp infer_expr_type_for_call(expr, _doc, _offset), do: infer_expr_type(expr)

  defp find_call_at_offset(nil, _name, _offset), do: nil

  defp find_call_at_offset({:program, %{functions: functions}}, name, offset) do
    Enum.reduce_while(functions, nil, fn {:function, %{body: body}}, _acc ->
      case find_call_in_block(body, name, offset) do
        nil -> {:cont, nil}
        call -> {:halt, call}
      end
    end)
  end

  defp find_call_in_block(nil, _name, _offset), do: nil

  defp find_call_in_block({:block, %{stmts: stmts}}, name, offset) do
    Enum.reduce_while(stmts, nil, fn stmt, _acc ->
      case find_call_in_stmt(stmt, name, offset) do
        nil -> {:cont, nil}
        call -> {:halt, call}
      end
    end)
  end

  defp find_call_in_stmt({:let, %{expr: expr}}, name, offset), do: find_call_in_expr(expr, name, offset)
  defp find_call_in_stmt({:assign, %{expr: expr}}, name, offset), do: find_call_in_expr(expr, name, offset)
  defp find_call_in_stmt({:return, %{expr: expr}}, name, offset), do: find_call_in_expr(expr, name, offset)
  defp find_call_in_stmt({:expr, %{expr: expr}}, name, offset), do: find_call_in_expr(expr, name, offset)
  defp find_call_in_stmt({:if_stmt, %{then_block: then_block, else_branch: else_branch}}, name, offset) do
    find_call_in_block(then_block, name, offset) || find_call_in_else(else_branch, name, offset)
  end

  defp find_call_in_stmt({:while, %{body: body}}, name, offset), do: find_call_in_block(body, name, offset)
  defp find_call_in_stmt({:loop, %{body: body}}, name, offset), do: find_call_in_block(body, name, offset)
  defp find_call_in_stmt({:for, %{body: body}}, name, offset), do: find_call_in_block(body, name, offset)
  defp find_call_in_stmt({:guard, %{else_block: block}}, name, offset), do: find_call_in_block(block, name, offset)
  defp find_call_in_stmt(_stmt, _name, _offset), do: nil

  defp find_call_in_else(nil, _name, _offset), do: nil
  defp find_call_in_else({:else_block, %{block: block}}, name, offset), do: find_call_in_block(block, name, offset)
  defp find_call_in_else({:else_if, %{if: if_stmt}}, name, offset), do: find_call_in_stmt(if_stmt, name, offset)

  defp find_call_in_expr(nil, _name, _offset), do: nil

  defp find_call_in_expr({:call, %{name: name, args: args, span: span}}, name, offset) do
    if span_contains?(span, offset), do: {:call, args}, else: nil
  end

  defp find_call_in_expr({:call, %{args: args}}, name, offset) do
    Enum.find_value(args, &find_call_in_expr(&1, name, offset))
  end

  defp find_call_in_expr({:method_call, %{target: target, args: args}}, name, offset) do
    find_call_in_expr(target, name, offset) || Enum.find_value(args, &find_call_in_expr(&1, name, offset))
  end

  defp find_call_in_expr({:binary, %{left: left, right: right}}, name, offset) do
    find_call_in_expr(left, name, offset) || find_call_in_expr(right, name, offset)
  end

  defp find_call_in_expr({:block_expr, %{block: block}}, name, offset), do: find_call_in_block(block, name, offset)
  defp find_call_in_expr({:match, %{cases: cases}}, name, offset) do
    Enum.find_value(cases, fn %{body: body} -> find_call_in_expr(body, name, offset) end)
  end

  defp find_call_in_expr({:if_expr, %{then_block: then_block, else_branch: else_branch}}, name, offset) do
    find_call_in_block(then_block, name, offset) || find_call_in_else(else_branch, name, offset)
  end

  defp find_call_in_expr({:struct, %{fields: fields}}, name, offset) do
    Enum.find_value(fields, fn %{expr: expr} -> find_call_in_expr(expr, name, offset) end)
  end

  defp find_call_in_expr({:res_ok, %{expr: expr}}, name, offset), do: find_call_in_expr(expr, name, offset)
  defp find_call_in_expr({:res_err, %{expr: expr}}, name, offset), do: find_call_in_expr(expr, name, offset)
  defp find_call_in_expr({:opt_some, %{expr: expr}}, name, offset), do: find_call_in_expr(expr, name, offset)
  defp find_call_in_expr(_expr, _name, _offset), do: nil

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

  defp hover_in_interpolated_string(doc, offset) do
    case token_at(doc.tokens, offset) do
      %BeamLang.Token{type: :string, value: value, span: span} ->
        rel_offset = offset - span.start - 1

        case interpolation_expr_at(value, rel_offset) do
          nil ->
            :no_interpolation

          {expr_str, expr_offset} ->
            expr_str = String.trim(expr_str)
            expr_offset = max(expr_offset, 0)

            cond do
              expr_str == "" ->
                :no_interpolation

              interpolation_field_access(expr_str) ->
                {target, field} = interpolation_field_parts(expr_str)
                arrow_idx =
                  case :binary.match(expr_str, "->") do
                    {idx, _} -> idx
                    :nomatch -> 0
                  end

                if expr_offset <= arrow_idx do
                  case lookup_hover(doc, target, offset) do
                    {:ok, info} -> {:ok, info}
                    _ -> {:ok, target}
                  end
                else
                  case interpolation_field_info(doc, target, field, offset) do
                    nil -> {:ok, expr_str}
                    info -> {:ok, info}
                  end
                end

              identifier = interpolation_identifier(expr_str) ->
                case lookup_hover(doc, identifier, offset) do
                  {:ok, info} -> {:ok, info}
                  _ -> {:ok, expr_str}
                end

              true ->
                {:ok, expr_str}
            end
        end

      _ ->
        :no_string
    end
  end

  defp interpolation_field_access(expr_str) do
    Regex.match?(~r/^[A-Za-z_][A-Za-z0-9_]*->[A-Za-z_][A-Za-z0-9_]*$/, expr_str)
  end

  defp interpolation_field_parts(expr_str) do
    case Regex.run(~r/^([A-Za-z_][A-Za-z0-9_]*)->([A-Za-z_][A-Za-z0-9_]*)$/, expr_str) do
      [_, target, field] -> {target, field}
      _ -> {nil, nil}
    end
  end

  defp interpolation_field_info(doc, target, field, offset) when is_binary(target) and is_binary(field) do
    target_type =
      case lookup_local(doc, target, offset) do
        {:ok, info} -> info.type
        :error -> nil
      end

    case lookup_field_type(doc, target_type, field) do
      {:ok, type_name, field_type} ->
        "field #{field}: #{format_type(field_type)} (on #{type_name})"

      :error ->
        nil
    end
  end

  defp interpolation_field_info(_doc, _target, _field, _offset), do: nil

  defp interpolation_identifier(expr_str) do
    if Regex.match?(~r/^[A-Za-z_][A-Za-z0-9_]*$/, expr_str) do
      expr_str
    else
      nil
    end
  end

  defp lookup_field_type(_doc, nil, _field), do: :error

  defp lookup_field_type(doc, {:named, type_name}, field) do
    lookup_named_field(doc, type_name, field)
  end

  defp lookup_field_type(doc, type_name, field) when is_binary(type_name) do
    lookup_named_field(doc, type_name, field)
  end

  defp lookup_field_type(_doc, _type, _field), do: :error

  defp lookup_named_field(doc, type_name, field) do
    case Map.get(doc.index[:types] || %{}, type_name) do
      %{fields: fields} ->
        case Enum.find(fields, fn %{name: name} -> name == field end) do
          nil -> lookup_error_field(doc, type_name, field)
          %{type: type} -> {:ok, type_name, type}
        end

      _ ->
        lookup_error_field(doc, type_name, field)
    end
  end

  defp lookup_error_field(doc, type_name, field) do
    case Map.get(doc.index[:errors] || %{}, type_name) do
      %{fields: fields} ->
        case Enum.find(fields, fn %{name: name} -> name == field end) do
          nil -> :error
          %{type: type} -> {:ok, type_name, type}
        end

      _ ->
        :error
    end
  end

  defp interpolation_expr_at(value, rel_offset) when is_integer(rel_offset) do
    if rel_offset < 0 or rel_offset >= byte_size(value) do
      nil
    else
      find_interpolation_at(value, rel_offset, 0)
    end
  end

  defp find_interpolation_at(<<"${", rest::binary>>, rel_offset, idx) do
    case find_closing_brace(rest, 0, "") do
      {:ok, expr, remaining} ->
        expr_start = idx + 2
        expr_end = expr_start + byte_size(expr)

        cond do
          rel_offset >= expr_start and rel_offset <= expr_end ->
            {expr, rel_offset - expr_start}

          true ->
            consumed = byte_size(rest) - byte_size(remaining)
            next_idx = idx + 2 + consumed
            find_interpolation_at(remaining, rel_offset, next_idx)
        end

      {:error, _} ->
        nil
    end
  end

  defp find_interpolation_at(<<_char::utf8, rest::binary>>, rel_offset, idx) do
    find_interpolation_at(rest, rel_offset, idx + 1)
  end

  defp find_interpolation_at(<<>>, _rel_offset, _idx), do: nil

  defp find_closing_brace(<<"}", rest::binary>>, 0, acc), do: {:ok, acc, rest}
  defp find_closing_brace(<<"}", rest::binary>>, depth, acc), do: find_closing_brace(rest, depth - 1, acc <> "}")
  defp find_closing_brace(<<"{", rest::binary>>, depth, acc), do: find_closing_brace(rest, depth + 1, acc <> "{")
  defp find_closing_brace(<<char::utf8, rest::binary>>, depth, acc), do: find_closing_brace(rest, depth, acc <> <<char::utf8>>)
  defp find_closing_brace(<<>>, _depth, _acc), do: {:error, :unclosed}

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

  defp collect_block_locals(nil, _func_span, _func_table, env), do: {[], env}

  defp collect_block_locals({:block, %{stmts: stmts, span: block_span}}, func_span, func_table, env) do
    Enum.reduce(stmts, {[], env}, fn stmt, {locals, acc_env} ->
      {new_locals, next_env} = collect_stmt_locals(stmt, func_span, block_span, func_table, acc_env)
      {locals ++ new_locals, next_env}
    end)
  end

  defp collect_stmt_locals({:let, %{name: name, type: type, expr: expr, span: span} = info}, func_span, scope_span, func_table, env) do
    inferred =
      case Map.get(info, :inferred_type) do
        nil -> type || infer_expr_type_with_env(expr, func_table, env)
        :any -> type || infer_expr_type_with_env(expr, func_table, env) || :any
        other -> other
      end
    entry = %{name: name, type: inferred, span: span, func_span: func_span, scope_span: scope_span, kind: :let}
    expr_locals = collect_expr_locals(expr, func_span, scope_span, func_table, env)
    {expr_locals ++ [entry], Map.put(env, name, inferred)}
  end

  defp collect_stmt_locals({:for, %{name: name, body: body, span: span}}, func_span, _scope_span, func_table, env) do
    entry = %{name: name, type: nil, span: span, func_span: func_span, scope_span: span, kind: :for}
    {body_locals, _} = collect_block_locals(body, func_span, func_table, Map.put(env, name, nil))
    {[entry | body_locals], env}
  end

  defp collect_stmt_locals({:if_stmt, %{then_block: then_block, else_branch: else_branch}}, func_span, _scope_span, func_table, env) do
    {then_locals, _} = collect_block_locals(then_block, func_span, func_table, env)
    else_locals = collect_else_locals(else_branch, func_span, func_table, env)
    {then_locals ++ else_locals, env}
  end

  defp collect_stmt_locals({:while, %{body: body}}, func_span, _scope_span, func_table, env) do
    {body_locals, _} = collect_block_locals(body, func_span, func_table, env)
    {body_locals, env}
  end

  defp collect_stmt_locals({:loop, %{body: body}}, func_span, _scope_span, func_table, env) do
    {body_locals, _} = collect_block_locals(body, func_span, func_table, env)
    {body_locals, env}
  end

  defp collect_stmt_locals({:guard, %{else_block: block}}, func_span, _scope_span, func_table, env) do
    {body_locals, _} = collect_block_locals(block, func_span, func_table, env)
    {body_locals, env}
  end

  defp collect_stmt_locals({:expr, %{expr: expr}}, func_span, scope_span, func_table, env) do
    {collect_expr_locals(expr, func_span, scope_span, func_table, env), env}
  end

  defp collect_stmt_locals(_stmt, _func_span, _scope_span, _func_table, env), do: {[], env}

  defp collect_else_locals(nil, _func_span, _func_table, _env), do: []

  defp collect_else_locals({:else_block, %{block: block}}, func_span, func_table, env) do
    {locals, _} = collect_block_locals(block, func_span, func_table, env)
    locals
  end

  defp collect_else_locals({:else_if, %{if: if_stmt}}, func_span, func_table, env) do
    {locals, _} = collect_stmt_locals(if_stmt, func_span, nil, func_table, env)
    locals
  end

  defp collect_expr_locals(nil, _func_span, _scope_span, _func_table, _env), do: []

  defp collect_expr_locals({:block_expr, %{block: block}}, func_span, _scope_span, func_table, env) do
    {locals, _} = collect_block_locals(block, func_span, func_table, env)
    locals
  end

  defp collect_expr_locals({:if_expr, %{then_block: then_block, else_branch: else_branch}}, func_span, _scope_span, func_table, env) do
    {then_locals, _} = collect_block_locals(then_block, func_span, func_table, env)
    else_locals = collect_else_locals(else_branch, func_span, func_table, env)
    then_locals ++ else_locals
  end

  defp collect_expr_locals({:match, %{expr: match_expr, cases: cases}}, func_span, _scope_span, func_table, env) do
    match_type =
      match_expr
      |> infer_expr_type_with_env(func_table, env)
      |> normalize_match_type()

    Enum.flat_map(cases, fn %{pattern: pattern, body: body, span: case_span} ->
      pattern_locals =
        pattern
        |> pattern_bindings_with_type(match_type)
        |> Enum.map(fn {name, span, type} ->
          %{name: name, type: type, span: span, func_span: func_span, scope_span: case_span, kind: :let}
        end)

      pattern_locals ++ collect_expr_locals(body, func_span, case_span, func_table, env)
    end)
  end

  defp collect_expr_locals({:call, %{args: args}}, func_span, scope_span, func_table, env) do
    Enum.flat_map(args, &collect_expr_locals(&1, func_span, scope_span, func_table, env))
  end

  defp collect_expr_locals({:method_call, %{target: target, args: args}}, func_span, scope_span, func_table, env) do
    collect_expr_locals(target, func_span, scope_span, func_table, env) ++
      Enum.flat_map(args, &collect_expr_locals(&1, func_span, scope_span, func_table, env))
  end

  defp collect_expr_locals({:binary, %{left: left, right: right}}, func_span, scope_span, func_table, env) do
    collect_expr_locals(left, func_span, scope_span, func_table, env) ++
      collect_expr_locals(right, func_span, scope_span, func_table, env)
  end

  defp collect_expr_locals({:struct, %{fields: fields}}, func_span, scope_span, func_table, env) do
    Enum.flat_map(fields, fn %{expr: expr} -> collect_expr_locals(expr, func_span, scope_span, func_table, env) end)
  end

  defp collect_expr_locals(_expr, _func_span, _scope_span, _func_table, _env), do: []

  defp pattern_bindings_with_type({:pat_identifier, %{name: name, span: span}}, type),
    do: [{name, span, type}]

  defp pattern_bindings_with_type({:opt_some_pat, %{name: name, span: span}}, {:optional, inner}),
    do: [{name, span, inner}]

  defp pattern_bindings_with_type({:opt_some_pat, %{name: name, span: span}}, _),
    do: [{name, span, nil}]

  defp pattern_bindings_with_type({:res_ok_pat, %{name: name, span: span}}, {:result, ok_type, _err_type}),
    do: [{name, span, ok_type}]

  defp pattern_bindings_with_type({:res_ok_pat, %{name: name, span: span}}, _),
    do: [{name, span, nil}]

  defp pattern_bindings_with_type({:res_err_pat, %{name: name, span: span}}, {:result, _ok_type, err_type}),
    do: [{name, span, err_type}]

  defp pattern_bindings_with_type({:res_err_pat, %{name: name, span: span}}, _),
    do: [{name, span, nil}]

  defp pattern_bindings_with_type({:struct_pattern, %{name: type_name, fields: fields}}, _type) do
    Enum.flat_map(fields, fn %{pattern: pat} -> pattern_bindings_with_type(pat, {:named, type_name}) end)
  end
  defp pattern_bindings_with_type(_pattern, _type), do: []

  defp normalize_match_type({:generic, {:named, "Result"}, [ok_type, err_type]}),
    do: {:result, ok_type, err_type}

  defp normalize_match_type({:generic, {:named, "Optional"}, [inner]}),
    do: {:optional, inner}

  defp normalize_match_type(other), do: other

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

  defp infer_expr_type_with_env({:identifier, %{name: name}}, _func_table, env) do
    Map.get(env, name)
  end

  defp infer_expr_type_with_env({:call, %{name: name}}, func_table, _env) do
    case Map.get(func_table, name) do
      nil -> nil
      info -> info.return_type
    end
  end

  defp infer_expr_type_with_env({:method_call, %{target: target, name: name}}, func_table, env) do
    case infer_expr_type_with_env(target, func_table, env) do
      {:generic, {:named, "List"}, [elem_type]} when name == "first" ->
        {:optional, elem_type}

      {:generic, {:named, "Iterator"}, [elem_type]} when name == "next" ->
        {:optional, elem_type}

      other ->
        other
    end
  end

  defp infer_expr_type_with_env(expr, _func_table, _env), do: infer_expr_type(expr)

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
