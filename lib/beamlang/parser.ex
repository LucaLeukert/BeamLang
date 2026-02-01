defmodule BeamLang.Parser do
  @moduledoc """
  Parses BeamLang tokens into an AST.
  """

  alias BeamLang.Token

  @spec parse([Token.t()]) :: {:ok, BeamLang.AST.t()} | {:error, BeamLang.Error.t()}
  def parse(tokens) when is_list(tokens) do
    with {:ok, ast, []} <- parse_program(tokens) do
      {:ok, ast}
    else
      {:ok, _ast, rest} -> {:error, unexpected(rest)}
      {:error, _} = error -> error
    end
  end

  @spec parse_program([Token.t()]) ::
          {:ok, BeamLang.AST.t(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_program(tokens) do
    parse_decls(tokens, [], [], [], [])
  end

  @spec parse_decls([Token.t()], [BeamLang.AST.import()], [BeamLang.AST.type_def()], [BeamLang.AST.error_def()], [BeamLang.AST.func()]) ::
          {:ok, BeamLang.AST.t(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_decls([], [], [], [], []) do
    {:error, BeamLang.Error.new(:parser, "Expected at least one declaration.", eof_span("<source>"))}
  end

  defp parse_decls([], imports, types, errors, functions) do
    imports = Enum.reverse(imports)
    types = Enum.reverse(types)
    errors = Enum.reverse(errors)
    functions = Enum.reverse(functions)
    span = program_span(imports, types, functions)
    {:ok, {:program, %{module: nil, imports: imports, types: types, errors: errors, functions: functions, span: span}}, []}
  end

  defp parse_decls([%Token{type: :import_kw} | _] = tokens, imports, types, errors, functions) do
    with {:ok, import_stmt, rest} <- parse_import(tokens) do
      parse_decls(rest, [import_stmt | imports], types, errors, functions)
    end
  end

  defp parse_decls([%Token{type: :export_kw} | rest] = tokens, imports, types, errors, functions) do
    case rest do
      [%Token{type: :internal_kw} | _] ->
        {:error, error("Internal functions cannot be exported.", hd(rest))}

      [%Token{type: :type_kw} | _] ->
        with {:ok, _export_tok, rest1} <- expect(tokens, :export_kw),
             {:ok, type_def, rest2} <- parse_type_def(rest1, true) do
          parse_decls(rest2, imports, [type_def | types], errors, functions)
        end

      [%Token{type: :error_kw} | _] ->
        with {:ok, _export_tok, rest1} <- expect(tokens, :export_kw),
             {:ok, error_def, rest2} <- parse_error_def(rest1, true) do
          parse_decls(rest2, imports, types, [error_def | errors], functions)
        end

      [%Token{type: :at} | _] ->
        with {:ok, _export_tok, rest1} <- expect(tokens, :export_kw),
             {:ok, external, rest2} <- parse_external_attr(rest1),
             {:ok, func, rest3} <- parse_function(rest2, external, true, false) do
          parse_decls(rest3, imports, types, errors, [func | functions])
        end

      [%Token{type: :fn} | _] ->
        with {:ok, _export_tok, rest1} <- expect(tokens, :export_kw),
             {:ok, func, rest2} <- parse_function(rest1, nil, true, false) do
          parse_decls(rest2, imports, types, errors, [func | functions])
        end

      _ ->
        {:error, error("Expected type, error, or fn after export.", hd(tokens))}
    end
  end

  defp parse_decls([%Token{type: :internal_kw} | rest] = tokens, imports, types, errors, functions) do
    case rest do
      [%Token{type: :type_kw} | _] ->
        {:error, error("Expected fn after internal.", hd(rest))}

      [%Token{type: :export_kw} | _] ->
        {:error, error("Internal functions cannot be exported.", hd(rest))}

      [%Token{type: :at} | _] ->
        with {:ok, _internal_tok, rest1} <- expect(tokens, :internal_kw),
             {:ok, external, rest2} <- parse_external_attr(rest1),
             {:ok, func, rest3} <- parse_function(rest2, external, false, true) do
          parse_decls(rest3, imports, types, errors, [func | functions])
        end

      [%Token{type: :fn} | _] ->
        with {:ok, _internal_tok, rest1} <- expect(tokens, :internal_kw),
             {:ok, func, rest2} <- parse_function(rest1, nil, false, true) do
          parse_decls(rest2, imports, types, errors, [func | functions])
        end

      _ ->
        {:error, error("Expected fn after internal.", hd(tokens))}
    end
  end

  defp parse_decls([%Token{type: :type_kw} | _] = tokens, imports, types, errors, functions) do
    with {:ok, type_def, rest} <- parse_type_def(tokens, false) do
      parse_decls(rest, imports, [type_def | types], errors, functions)
    end
  end

  defp parse_decls([%Token{type: :error_kw} | _] = tokens, imports, types, errors, functions) do
    with {:ok, error_def, rest} <- parse_error_def(tokens, false) do
      parse_decls(rest, imports, types, [error_def | errors], functions)
    end
  end

  defp parse_decls([%Token{type: :at} | _] = tokens, imports, types, errors, functions) do
    with {:ok, external, rest1} <- parse_external_attr(tokens),
         {:ok, func, rest2} <- parse_function(rest1, external, false, false) do
      parse_decls(rest2, imports, types, errors, [func | functions])
    end
  end

  defp parse_decls(tokens, imports, types, errors, functions) do
    with {:ok, func, rest} <- parse_function(tokens, nil, false, false) do
      parse_decls(rest, imports, types, errors, [func | functions])
    end
  end

  @spec parse_function([Token.t()], map() | nil, boolean(), boolean()) ::
          {:ok, BeamLang.AST.func(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_function(tokens, external, exported, internal) do
    with {:ok, _fn_tok, rest1} <- expect(tokens, :fn),
         {:ok, name_tok, rest2} <- expect(rest1, :identifier),
         {:ok, {type_params, params_span}, rest3} <- parse_type_params(rest2),
         {:ok, _lparen, rest4} <- expect(rest3, :lparen),
         {:ok, params, rest5} <- parse_params(rest4, []),
         {:ok, _rparen, rest6} <- expect(rest5, :rparen),
         {:ok, _arrow, rest7} <- expect(rest6, :arrow),
         {:ok, {return_type, return_span}, rest8} <- parse_type_name(rest7) do
      case external do
        nil ->
          with {:ok, _lbrace, rest9} <- expect(rest8, :lbrace),
               {:ok, body, rest10} <- parse_block(rest9),
               {:ok, _rbrace, rest11} <- expect(rest10, :rbrace) do
            span = BeamLang.Span.merge(name_tok.span, return_span)
            span =
              case params_span do
                nil -> span
                _ -> BeamLang.Span.merge(span, params_span)
              end
            span = BeamLang.Span.merge(span, rbrace_span(rest10, rest11))

            ast =
              {:function,
               %{
                 name: name_tok.value,
                 type_params: type_params,
                 params: params,
                 return_type: return_type,
                 body: body,
                 external: nil,
                 exported: exported,
                 internal: internal,
                 span: span
               }}

            {:ok, ast, rest11}
          end

        _ ->
          with {:ok, semi_tok, rest9} <- expect(rest8, :semicolon) do
            span = BeamLang.Span.merge(name_tok.span, return_span)
            span =
              case params_span do
                nil -> span
                _ -> BeamLang.Span.merge(span, params_span)
              end
            span = BeamLang.Span.merge(span, semi_tok.span)

            ast =
              {:function,
               %{
                 name: name_tok.value,
                 type_params: type_params,
                 params: params,
                 return_type: return_type,
                 body: nil,
                 external: external,
                 exported: exported,
                 internal: internal,
                 span: span
               }}

            {:ok, ast, rest9}
          end
      end
    end
  end

  @spec parse_import([Token.t()]) ::
          {:ok, BeamLang.AST.import(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_import(tokens) do
    with {:ok, import_tok, rest1} <- expect(tokens, :import_kw),
         {:ok, module_tok, rest2} <- expect(rest1, :identifier) do
      {alias_name, rest3} = parse_optional_alias(rest2)

      {items, rest4} =
        case rest3 do
          [%Token{type: :dot} | rest_after] ->
            case parse_import_suffix(rest_after) do
              {:ok, items, rest5} -> {items, rest5}
              {:error, _} -> {{:error, rest3}, rest3}
            end

          _ ->
            {:none, rest3}
        end

      case items do
        {:error, _} ->
          {:error, error("Expected import item.", hd(rest3))}

        _ ->
          {rest9, span} =
            case rest4 do
              [%Token{type: :semicolon} = semi | rest_after] ->
                {rest_after, BeamLang.Span.merge(import_tok.span, semi.span)}

              _ ->
                {rest4, BeamLang.Span.merge(import_tok.span, module_tok.span)}
            end

          {:ok, {:import, %{module: module_tok.value, alias: alias_name, items: items, span: span}}, rest9}
      end
    end
  end

  @spec parse_import_suffix([Token.t()]) ::
          {:ok, :all | [BeamLang.AST.import_item()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_import_suffix([%Token{type: :identifier} = tok | rest]) do
    {:ok, [%{name: tok.value, span: tok.span}], rest}
  end

  defp parse_import_suffix([%Token{type: :star} | rest]) do
    {:ok, :all, rest}
  end

  defp parse_import_suffix([%Token{type: :lbrace} | rest]) do
    with {:ok, items, rest1} <- parse_import_items(rest),
         {:ok, _rbrace, rest2} <- expect(rest1, :rbrace) do
      {:ok, items, rest2}
    end
  end

  defp parse_import_suffix([%Token{} = tok | _]) do
    {:error, error("Expected import item.", tok)}
  end

  @spec parse_import_items([Token.t()]) ::
          {:ok, :all | [BeamLang.AST.import_item()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_import_items([%Token{type: :star} | rest]) do
    {:ok, :all, rest}
  end

  defp parse_import_items(tokens) do
    parse_import_item_list(tokens, [])
  end

  @spec parse_import_item_list([Token.t()], [BeamLang.AST.import_item()]) ::
          {:ok, [BeamLang.AST.import_item()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_import_item_list([%Token{type: :identifier} = tok | rest], acc) do
    item = %{name: tok.value, span: tok.span}
    case rest do
      [%Token{type: :comma} | rest2] -> parse_import_item_list(rest2, [item | acc])
      _ -> {:ok, Enum.reverse([item | acc]), rest}
    end
  end

  defp parse_import_item_list([%Token{} = tok | _], _acc) do
    {:error, error("Expected import item.", tok)}
  end

  @spec parse_optional_alias([Token.t()]) :: {binary() | nil, [Token.t()]}
  defp parse_optional_alias([%Token{type: :as_kw} | rest]) do
    case rest do
      [%Token{type: :identifier} = tok | rest2] -> {tok.value, rest2}
      _ -> {nil, rest}
    end
  end

  defp parse_optional_alias(tokens), do: {nil, tokens}

  @spec parse_external_attr([Token.t()]) ::
          {:ok, map(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_external_attr(tokens) do
    with {:ok, _at_tok, rest1} <- expect(tokens, :at),
         {:ok, name_tok, rest2} <- expect(rest1, :identifier),
         {:ok, _lparen, rest3} <- expect(rest2, :lparen),
         {:ok, args, rest4} <- parse_external_args(rest3, []),
         {:ok, _rparen, rest5} <- expect(rest4, :rparen) do
      if name_tok.value == "external" and length(args) == 3 do
        [language, mod, fun] = args
        {:ok, %{language: language, module: mod, function: fun}, rest5}
      else
        {:error, BeamLang.Error.new(:parser, "Invalid external declaration.", name_tok.span)}
      end
    end
  end


  @spec parse_external_args([Token.t()], [binary()]) ::
          {:ok, [binary()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_external_args([%Token{type: type} = tok | rest], acc) when type in [:identifier, :string] do
    value = tok.value

    case rest do
      [%Token{type: :comma} | rest2] -> parse_external_args(rest2, [value | acc])
      _ -> {:ok, Enum.reverse([value | acc]), rest}
    end
  end

  defp parse_external_args([%Token{} = tok | _], _acc) do
    {:error, BeamLang.Error.new(:parser, "Expected external argument.", tok.span)}
  end

  @spec parse_block([Token.t()]) ::
          {:ok, BeamLang.AST.block(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_block(tokens) do
    with {:ok, stmts, rest} <- parse_statements(tokens, []) do
      {:ok, {:block, %{stmts: stmts, span: block_span(stmts)}}, rest}
    end
  end

  @spec parse_statements([Token.t()], [BeamLang.AST.stmt()]) ::
          {:ok, [BeamLang.AST.stmt()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_statements([%Token{type: :rbrace} | _] = rest, acc) do
    {:ok, Enum.reverse(acc), rest}
  end

  defp parse_statements(tokens, acc) do
    case parse_statement(tokens) do
      {:ok, stmt, rest} -> parse_statements(rest, [stmt | acc])
      {:error, _} = error -> error
    end
  end

  @spec parse_statement([Token.t()]) :: {:ok, BeamLang.AST.stmt(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_statement([%Token{type: :return} | _] = tokens) do
    parse_return(tokens)
  end

  defp parse_statement([%Token{type: :let} | _] = tokens) do
    parse_let(tokens)
  end

  defp parse_statement([%Token{type: :match} | _] = tokens) do
    parse_match_statement(tokens)
  end

  defp parse_statement([%Token{type: :if_kw} | _] = tokens) do
    parse_if_statement(tokens)
  end

  defp parse_statement([%Token{type: :while_kw} | _] = tokens) do
    parse_while(tokens)
  end

  defp parse_statement([%Token{type: :loop_kw} | _] = tokens) do
    parse_loop(tokens)
  end

  defp parse_statement([%Token{type: :for_kw} | _] = tokens) do
    parse_for(tokens)
  end

  defp parse_statement([%Token{type: :break_kw} | _] = tokens) do
    parse_break(tokens)
  end

  defp parse_statement([%Token{type: :guard} | _] = tokens) do
    parse_guard(tokens)
  end

  defp parse_statement([%Token{type: :identifier} | _] = tokens) do
    case parse_lvalue(tokens) do
      {:ok, target, [%Token{type: :equals} = eq_tok | rest]} ->
        parse_assignment(target, eq_tok, rest)

      _ ->
        parse_expr_statement(tokens)
    end
  end

  defp parse_statement(tokens) do
    parse_expr_statement(tokens)
  end

  defp parse_expr_statement(tokens) do
    with {:ok, expr, rest1} <- parse_expression(tokens),
         {:ok, _semi, rest2} <- expect(rest1, :semicolon) do
      span = BeamLang.Span.merge(expr_span(expr), semicolon_span(rest1, rest2))
      {:ok, {:expr, %{expr: expr, span: span}}, rest2}
    end
  end

  @spec parse_match_statement([Token.t()]) ::
          {:ok, BeamLang.AST.stmt(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_match_statement(tokens) do
    with {:ok, expr, rest1} <- parse_expression(tokens) do
      case rest1 do
        [%Token{type: :semicolon} | _] ->
          with {:ok, _semi, rest2} <- expect(rest1, :semicolon) do
            span = BeamLang.Span.merge(expr_span(expr), semicolon_span(rest1, rest2))
            {:ok, {:expr, %{expr: expr, span: span}}, rest2}
          end

        _ ->
          {:ok, {:expr, %{expr: expr, span: expr_span(expr)}}, rest1}
      end
    end
  end

  @spec parse_guard([Token.t()]) :: {:ok, BeamLang.AST.stmt(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_guard(tokens) do
    with {:ok, guard_tok, rest1} <- expect(tokens, :guard),
         {:ok, _lparen, rest2} <- expect(rest1, :lparen),
         {:ok, cond, rest3} <- parse_expression(rest2),
         {:ok, _rparen, rest4} <- expect(rest3, :rparen),
         {:ok, _else_tok, rest5} <- expect(rest4, :else_kw),
         {:ok, _lbrace, rest6} <- expect(rest5, :lbrace),
         {:ok, else_block, rest7} <- parse_block(rest6),
         {:ok, _rbrace, rest8} <- expect(rest7, :rbrace) do
      span = BeamLang.Span.merge(guard_tok.span, expr_span(cond))
      span = BeamLang.Span.merge(span, rbrace_span(rest7, rest8))
      {:ok, {:guard, %{cond: cond, else_block: else_block, span: span}}, rest8}
    end
  end

  @spec parse_if_statement([Token.t()]) ::
          {:ok, BeamLang.AST.stmt(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_if_statement(tokens) do
    with {:ok, if_tok, rest1} <- expect(tokens, :if_kw),
         {:ok, _lparen, rest2} <- expect(rest1, :lparen),
         {:ok, cond, rest3} <- parse_expression(rest2),
         {:ok, _rparen, rest4} <- expect(rest3, :rparen),
         {:ok, _lbrace, rest5} <- expect(rest4, :lbrace),
         {:ok, then_block, rest6} <- parse_block(rest5),
         {:ok, _rbrace, rest7} <- expect(rest6, :rbrace),
         {:ok, else_branch, rest8} <- parse_optional_else(rest7) do
      span = BeamLang.Span.merge(if_tok.span, branch_span(then_block, else_branch))
      {:ok, {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch, span: span}}, rest8}
    end
  end

  @spec parse_if_expr([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_if_expr(tokens) do
    with {:ok, if_tok, rest1} <- expect(tokens, :if_kw),
         {:ok, _lparen, rest2} <- expect(rest1, :lparen),
         {:ok, cond, rest3} <- parse_expression(rest2),
         {:ok, _rparen, rest4} <- expect(rest3, :rparen),
         {:ok, _lbrace, rest5} <- expect(rest4, :lbrace),
         {:ok, then_block, rest6} <- parse_block(rest5),
         {:ok, _rbrace, rest7} <- expect(rest6, :rbrace),
         {:ok, else_branch, rest8} <- parse_required_else_expr(rest7) do
      span = BeamLang.Span.merge(if_tok.span, branch_span(then_block, else_branch))
      {:ok, {:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch, span: span}}, rest8}
    end
  end

  @spec parse_optional_else([Token.t()]) ::
          {:ok, BeamLang.AST.if_else_branch() | nil, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_optional_else([%Token{type: :else_kw} | rest]) do
    parse_else_branch(rest)
  end

  defp parse_optional_else(tokens), do: {:ok, nil, tokens}

  @spec parse_required_else_expr([Token.t()]) ::
          {:ok, BeamLang.AST.if_else_branch(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_required_else_expr([%Token{type: :else_kw} | rest]) do
    parse_else_branch(rest)
  end

  defp parse_required_else_expr([%Token{} = tok | _]) do
    {:error, error("Expected else clause for if expression.", tok)}
  end

  @spec parse_else_branch([Token.t()]) ::
          {:ok, BeamLang.AST.if_else_branch(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_else_branch([%Token{type: :if_kw} | _] = tokens) do
    with {:ok, if_stmt, rest} <- parse_if_statement(tokens) do
      span = stmt_span(if_stmt)
      {:ok, {:else_if, %{if: if_stmt, span: span}}, rest}
    end
  end

  defp parse_else_branch([%Token{type: :lbrace} | rest]) do
    with {:ok, block, rest1} <- parse_block(rest),
         {:ok, _rbrace, rest2} <- expect(rest1, :rbrace) do
      span = block_span(block)
      {:ok, {:else_block, %{block: block, span: span}}, rest2}
    end
  end

  defp parse_else_branch([%Token{} = tok | _]) do
    {:error, error("Expected else branch.", tok)}
  end

  @spec parse_while([Token.t()]) :: {:ok, BeamLang.AST.stmt(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_while(tokens) do
    with {:ok, while_tok, rest1} <- expect(tokens, :while_kw),
         {:ok, _lparen, rest2} <- expect(rest1, :lparen),
         {:ok, cond, rest3} <- parse_expression(rest2),
         {:ok, _rparen, rest4} <- expect(rest3, :rparen),
         {:ok, _lbrace, rest5} <- expect(rest4, :lbrace),
         {:ok, body, rest6} <- parse_block(rest5),
         {:ok, _rbrace, rest7} <- expect(rest6, :rbrace) do
      span = BeamLang.Span.merge(while_tok.span, block_span(body))
      {:ok, {:while, %{cond: cond, body: body, span: span}}, rest7}
    end
  end

  @spec parse_loop([Token.t()]) :: {:ok, BeamLang.AST.stmt(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_loop(tokens) do
    with {:ok, loop_tok, rest1} <- expect(tokens, :loop_kw),
         {:ok, _lbrace, rest2} <- expect(rest1, :lbrace),
         {:ok, body, rest3} <- parse_block(rest2),
         {:ok, _rbrace, rest4} <- expect(rest3, :rbrace) do
      span = BeamLang.Span.merge(loop_tok.span, block_span(body))
      {:ok, {:loop, %{body: body, span: span}}, rest4}
    end
  end

  @spec parse_for([Token.t()]) :: {:ok, BeamLang.AST.stmt(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_for(tokens) do
    with {:ok, for_tok, rest1} <- expect(tokens, :for_kw),
         {:ok, _lparen, rest2} <- expect(rest1, :lparen),
         {:ok, name_tok, rest3} <- expect(rest2, :identifier),
         {:ok, _in_kw, rest4} <- expect(rest3, :in_kw),
         {:ok, collection, rest5} <- parse_expression(rest4),
         {:ok, _rparen, rest6} <- expect(rest5, :rparen),
         {:ok, _lbrace, rest7} <- expect(rest6, :lbrace),
         {:ok, body, rest8} <- parse_block(rest7),
         {:ok, _rbrace, rest9} <- expect(rest8, :rbrace) do
      span = BeamLang.Span.merge(for_tok.span, block_span(body))
      {:ok, {:for, %{name: name_tok.value, collection: collection, body: body, span: span}}, rest9}
    end
  end

  @spec parse_break([Token.t()]) :: {:ok, BeamLang.AST.stmt(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_break(tokens) do
    with {:ok, break_tok, rest1} <- expect(tokens, :break_kw),
         {:ok, _semi, rest2} <- expect(rest1, :semicolon) do
      span = BeamLang.Span.merge(break_tok.span, semicolon_span(rest1, rest2))
      {:ok, {:break, %{span: span}}, rest2}
    end
  end

  @spec parse_let([Token.t()]) :: {:ok, BeamLang.AST.stmt(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_let(tokens) do
    with {:ok, let_tok, rest1} <- expect(tokens, :let),
         {mutable, rest2} <- parse_mut(rest1),
         {:ok, name_tok, rest3} <- expect(rest2, :identifier),
         {type_ann, rest4} <- parse_optional_type(rest3),
         {:ok, _eq, rest5} <- expect(rest4, :equals),
         {:ok, expr, rest6} <- parse_expression(rest5),
         {:ok, _semi, rest7} <- expect(rest6, :semicolon) do
      span = BeamLang.Span.merge(let_tok.span, expr_span(expr))
      span = BeamLang.Span.merge(span, semicolon_span(rest6, rest7))
      {:ok,
       {:let,
        %{
          name: name_tok.value,
          mutable: mutable,
          type: type_ann,
          expr: expr,
          span: span
        }}, rest7}
    end
  end

  @spec parse_assignment(BeamLang.AST.expr(), Token.t(), [Token.t()]) ::
          {:ok, BeamLang.AST.stmt(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_assignment(target, eq_tok, tokens) do
    with {:ok, expr, rest1} <- parse_expression(tokens),
         {:ok, _semi, rest2} <- expect(rest1, :semicolon) do
      span = BeamLang.Span.merge(expr_span(target), eq_tok.span)
      span = BeamLang.Span.merge(span, semicolon_span(rest1, rest2))
      {:ok, {:assign, %{target: target, expr: expr, span: span}}, rest2}
    end
  end

  @spec parse_lvalue([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_lvalue(tokens) do
    with {:ok, expr, rest} <- parse_identifier(tokens) do
      parse_field_access(expr, rest)
    end
  end

  @spec parse_mut([Token.t()]) :: {boolean(), [Token.t()]}
  defp parse_mut([%Token{type: :mut} | rest]), do: {true, rest}
  defp parse_mut(tokens), do: {false, tokens}

  @spec parse_return([Token.t()]) ::
          {:ok, BeamLang.AST.stmt(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_return(tokens) do
    with {:ok, return_tok, rest1} <- expect(tokens, :return) do
      case rest1 do
        [%Token{type: :bang} | rest2] ->
          parse_tagged_return(:result, return_tok, rest2)

        [%Token{type: :question} | rest2] ->
          parse_tagged_return(:optional, return_tok, rest2)

        [%Token{type: :semicolon} | _] ->
          with {:ok, _semi, rest2} <- expect(rest1, :semicolon) do
            span = BeamLang.Span.merge(return_tok.span, semicolon_span(rest1, rest2))
            {:ok, {:return, %{expr: nil, span: span}}, rest2}
          end

        _ ->
          with {:ok, expr, rest2} <- parse_expression(rest1),
               {:ok, _semi, rest3} <- expect(rest2, :semicolon) do
            span = BeamLang.Span.merge(return_tok.span, expr_span(expr))
            span = BeamLang.Span.merge(span, semicolon_span(rest2, rest3))
            {:ok, {:return, %{expr: expr, span: span}}, rest3}
          end
      end
    end
  end

  @spec parse_tagged_return(:result | :optional, Token.t(), [Token.t()]) ::
          {:ok, BeamLang.AST.stmt(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_tagged_return(kind, return_tok, tokens) do
    with {:ok, tag_tok, rest1} <- expect(tokens, :identifier) do
      case {kind, tag_tok.value, rest1} do
        {:optional, "none", [%Token{type: :semicolon} | _]} ->
          with {:ok, _semi, rest2} <- expect(rest1, :semicolon) do
            expr = {:opt_none, %{span: tag_tok.span}}
            span = BeamLang.Span.merge(return_tok.span, semicolon_span(rest1, rest2))
            {:ok, {:return, %{expr: expr, span: span}}, rest2}
          end

        _ ->
          with {:ok, expr, rest2} <- parse_expression(rest1),
               {:ok, _semi, rest3} <- expect(rest2, :semicolon) do
            expr =
              case {kind, tag_tok.value} do
                {:result, "ok"} -> {:res_ok, %{expr: expr, span: expr_span(expr)}}
                {:result, "err"} -> {:res_err, %{expr: expr, span: expr_span(expr)}}
                {:optional, "some"} -> {:opt_some, %{expr: expr, span: expr_span(expr)}}
                _ -> {:invalid, %{span: tag_tok.span}}
              end

            case expr do
              {:invalid, %{span: span}} ->
                {:error, BeamLang.Error.new(:parser, "Invalid return tag.", span)}

              _ ->
                span = BeamLang.Span.merge(return_tok.span, expr_span(expr))
                span = BeamLang.Span.merge(span, semicolon_span(rest2, rest3))
                {:ok, {:return, %{expr: expr, span: span}}, rest3}
            end
          end
      end
    end
  end

  @spec parse_expression([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_expression(tokens) do
    parse_comparison(tokens)
  end

  @spec parse_comparison([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_comparison(tokens) do
    with {:ok, left, rest} <- parse_additive(tokens) do
      parse_comparison_tail(left, rest)
    end
  end

  @spec parse_comparison_tail(BeamLang.AST.expr(), [Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_comparison_tail(left, tokens) do
    case tokens do
      [%Token{type: type} | rest] ->
        case compare_op(type) do
          nil ->
            {:ok, left, tokens}

          op ->
            with {:ok, right, rest2} <- parse_additive(rest) do
              span = BeamLang.Span.merge(expr_span(left), expr_span(right))
              expr = {:binary, %{op: op, left: left, right: right, span: span}}
              parse_comparison_tail(expr, rest2)
            end
        end

      [] ->
        {:ok, left, []}
    end
  end

  @spec parse_additive([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_additive(tokens) do
    with {:ok, left, rest} <- parse_multiplicative(tokens) do
      parse_additive_tail(left, rest)
    end
  end

  @spec parse_additive_tail(BeamLang.AST.expr(), [Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_additive_tail(left, tokens) do
    case tokens do
      [%Token{type: type} | rest] ->
        case add_op(type) do
          nil ->
            {:ok, left, tokens}

          op ->
            with {:ok, right, rest2} <- parse_multiplicative(rest) do
              span = BeamLang.Span.merge(expr_span(left), expr_span(right))
              expr = {:binary, %{op: op, left: left, right: right, span: span}}
              parse_additive_tail(expr, rest2)
            end
        end

      [] ->
        {:ok, left, []}
    end
  end

  @spec parse_multiplicative([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_multiplicative(tokens) do
    with {:ok, left, rest} <- parse_term(tokens) do
      parse_multiplicative_tail(left, rest)
    end
  end

  @spec parse_multiplicative_tail(BeamLang.AST.expr(), [Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_multiplicative_tail(left, tokens) do
    case tokens do
      [%Token{type: type} | rest] ->
        case mul_op(type) do
          nil ->
            {:ok, left, tokens}

          op ->
            with {:ok, right, rest2} <- parse_term(rest) do
              span = BeamLang.Span.merge(expr_span(left), expr_span(right))
              expr = {:binary, %{op: op, left: left, right: right, span: span}}
              parse_multiplicative_tail(expr, rest2)
            end
        end

      [] ->
        {:ok, left, []}
    end
  end

  @spec parse_term([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_term(tokens) do
    with {:ok, expr, rest} <- parse_primary(tokens) do
      with {:ok, expr, rest1} <- parse_field_access(expr, rest) do
        parse_method_call(expr, rest1)
      end
    end
  end

  @spec parse_primary([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_primary([%Token{type: :match} | _] = tokens) do
    parse_match(tokens)
  end

  defp parse_primary([%Token{type: :if_kw} | _] = tokens) do
    parse_if_expr(tokens)
  end

  defp parse_primary([%Token{type: :fn} | _] = tokens) do
    parse_lambda(tokens)
  end

  defp parse_primary([%Token{type: :question} | rest]) do
    with {:ok, tag_tok, rest1} <- expect(rest, :identifier) do
      case tag_tok.value do
        "none" ->
          {:ok, {:opt_none, %{span: tag_tok.span}}, rest1}

        "some" ->
          with {:ok, expr, rest2} <- parse_expression(rest1) do
            span = BeamLang.Span.merge(tag_tok.span, expr_span(expr))
            {:ok, {:opt_some, %{expr: expr, span: span}}, rest2}
          end

        _ ->
          {:error, error("Invalid optional literal.", tag_tok)}
      end
    end
  end

  defp parse_primary([%Token{type: :bang} | rest]) do
    with {:ok, tag_tok, rest1} <- expect(rest, :identifier),
         {:ok, expr, rest2} <- parse_expression(rest1) do
      span = BeamLang.Span.merge(tag_tok.span, expr_span(expr))

      case tag_tok.value do
        "ok" -> {:ok, {:res_ok, %{expr: expr, span: span}}, rest2}
        "err" -> {:ok, {:res_err, %{expr: expr, span: span}}, rest2}
        _ -> {:error, error("Invalid result literal.", tag_tok)}
      end
    end
  end

  defp parse_primary([%Token{type: :lparen} | rest]) do
    with {:ok, expr, rest1} <- parse_expression(rest),
         {:ok, _rparen, rest2} <- expect(rest1, :rparen) do
      {:ok, expr, rest2}
    end
  end

  defp parse_primary([%Token{type: :lbracket} = lbracket_tok | rest]) do
    parse_list_literal(lbracket_tok, rest)
  end

  defp parse_primary(tokens) do
    case parse_call(tokens) do
      {:ok, _expr, _rest} = ok -> ok
      {:error, _} ->
        case parse_literal(tokens) do
          {:ok, _expr, _rest} = ok -> ok
          {:error, _} ->
            case parse_struct_literal(tokens) do
              {:ok, _expr, _rest} = ok -> ok
              {:error, _} -> parse_identifier(tokens)
            end
        end
    end
  end

  @spec parse_list_literal(Token.t(), [Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_list_literal(lbracket_tok, [%Token{type: :rbracket} = rbracket_tok | rest]) do
    span = BeamLang.Span.merge(lbracket_tok.span, rbracket_tok.span)
    {:ok, {:list_literal, %{elements: [], span: span}}, rest}
  end

  defp parse_list_literal(lbracket_tok, tokens) do
    with {:ok, elements, rest1} <- parse_list_elements(tokens, []),
         {:ok, rbracket_tok, rest2} <- expect(rest1, :rbracket) do
      span = BeamLang.Span.merge(lbracket_tok.span, rbracket_tok.span)
      {:ok, {:list_literal, %{elements: elements, span: span}}, rest2}
    end
  end

  @spec parse_list_elements([Token.t()], [BeamLang.AST.expr()]) ::
          {:ok, [BeamLang.AST.expr()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_list_elements(tokens, acc) do
    with {:ok, expr, rest} <- parse_expression(tokens) do
      case rest do
        [%Token{type: :comma} | rest1] ->
          parse_list_elements(rest1, acc ++ [expr])

        _ ->
          {:ok, acc ++ [expr], rest}
      end
    end
  end

  @spec parse_lambda([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_lambda(tokens) do
    with {:ok, fn_tok, rest1} <- expect(tokens, :fn),
         {:ok, _lparen, rest2} <- expect(rest1, :lparen),
         {:ok, params, rest3} <- parse_params(rest2, []),
         {:ok, _rparen, rest4} <- expect(rest3, :rparen),
         {:ok, _arrow, rest5} <- expect(rest4, :arrow),
         {:ok, {return_type, return_span}, rest6} <- parse_type_name(rest5),
         {:ok, _lbrace, rest7} <- expect(rest6, :lbrace),
         {:ok, body, rest8} <- parse_block(rest7),
         {:ok, rbrace, rest9} <- expect(rest8, :rbrace) do
      span = BeamLang.Span.merge(fn_tok.span, return_span)
      span = BeamLang.Span.merge(span, rbrace.span)
      {:ok, {:lambda, %{params: params, return_type: return_type, body: body, span: span}}, rest9}
    end
  end

  @spec parse_field_access(BeamLang.AST.expr(), [Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_field_access(expr, [%Token{type: :arrow} = arrow_tok, %Token{type: :identifier} = name_tok | rest]) do
    span = BeamLang.Span.merge(expr_span(expr), arrow_tok.span)
    span = BeamLang.Span.merge(span, name_tok.span)
    parse_field_access({:field, %{target: expr, name: name_tok.value, span: span}}, rest)
  end

  defp parse_field_access(expr, rest), do: {:ok, expr, rest}

  @spec parse_method_call(BeamLang.AST.expr(), [Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_method_call({:field, %{target: target, name: name, span: span}}, [%Token{type: :lparen} | rest]) do
    with {:ok, args, rest1} <- parse_args(rest, []),
         {:ok, rparen, rest2} <- expect(rest1, :rparen) do
      call_span = BeamLang.Span.merge(span, rparen.span)
      {:ok, {:method_call, %{target: target, name: name, args: args, span: call_span}}, rest2}
    end
  end

  defp parse_method_call(expr, rest), do: {:ok, expr, rest}

  @spec parse_call([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_call([%Token{type: :identifier} = mod_tok, %Token{type: :double_colon}, %Token{type: :identifier} = name_tok, %Token{type: :lparen} | rest]) do
    with {:ok, args, rest1} <- parse_args(rest, []),
         {:ok, rparen, rest2} <- expect(rest1, :rparen) do
      span = BeamLang.Span.merge(mod_tok.span, rparen.span)
      {:ok, {:call, %{name: "#{mod_tok.value}::#{name_tok.value}", args: args, span: span}}, rest2}
    end
  end

  @spec parse_call([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_call([%Token{type: :identifier} = name_tok, %Token{type: :lparen} = lparen | rest]) do
    with {:ok, args, rest1} <- parse_args(rest, []),
         {:ok, rparen, rest2} <- expect(rest1, :rparen) do
      span = BeamLang.Span.merge(name_tok.span, lparen.span)
      span = BeamLang.Span.merge(span, rparen.span)
      {:ok, {:call, %{name: name_tok.value, args: args, span: span}}, rest2}
    end
  end

  @spec parse_call([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_call([%Token{} = tok | _]) do
    {:error, error("Expected function call.", tok)}
  end

  @spec parse_params([Token.t()], [BeamLang.AST.func_param()]) ::
          {:ok, [BeamLang.AST.func_param()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_params([%Token{type: :rparen} | _] = rest, acc) do
    {:ok, Enum.reverse(acc), rest}
  end

  defp parse_params(tokens, acc) do
    with {:ok, name_tok, rest1} <- expect(tokens, :identifier),
         {:ok, _colon, rest2} <- expect(rest1, :colon),
         {:ok, {type_name, type_span}, rest3} <- parse_type_name(rest2) do
      span = BeamLang.Span.merge(name_tok.span, type_span)
      param = %{name: name_tok.value, type: type_name, span: span}

      case rest3 do
        [%Token{type: :comma} | rest4] -> parse_params(rest4, [param | acc])
        _ -> parse_params(rest3, [param | acc])
      end
    end
  end

  @spec parse_args([Token.t()], [BeamLang.AST.expr()]) ::
          {:ok, [BeamLang.AST.expr()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_args([%Token{type: :rparen} | _] = rest, acc) do
    {:ok, Enum.reverse(acc), rest}
  end

  defp parse_args(tokens, acc) do
    with {:ok, expr, rest1} <- parse_expression(tokens) do
      case rest1 do
        [%Token{type: :comma} | rest2] -> parse_args(rest2, [expr | acc])
        _ -> parse_args(rest1, [expr | acc])
      end
    end
  end

  @spec parse_match([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_match([%Token{type: :match} = match_tok | rest]) do
    with {:ok, _lparen, rest1} <- expect(rest, :lparen),
         {:ok, expr, rest2} <- parse_expression(rest1),
         {:ok, _rparen, rest3} <- expect(rest2, :rparen),
         {:ok, _lbrace, rest4} <- expect(rest3, :lbrace),
         {:ok, cases, rest5} <- parse_match_cases(rest4, []),
         {:ok, rbrace, rest6} <- expect(rest5, :rbrace) do
      span = BeamLang.Span.merge(match_tok.span, rbrace.span)
      {:ok, {:match, %{expr: expr, cases: cases, span: span}}, rest6}
    end
  end

  defp parse_match([%Token{} = tok | _]) do
    {:error, error("Expected match expression.", tok)}
  end

  @spec parse_match_cases([Token.t()], [BeamLang.AST.match_case()]) ::
          {:ok, [BeamLang.AST.match_case()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_match_cases([%Token{type: :rbrace} | _] = rest, acc) do
    {:ok, Enum.reverse(acc), rest}
  end

  defp parse_match_cases(tokens, acc) do
    with {:ok, case_tok, rest1} <- expect(tokens, :case),
         {:ok, pattern, rest2} <- parse_case_pattern(rest1),
         {:ok, guard, rest3} <- parse_optional_guard(rest2),
         {:ok, _arrow, rest4} <- expect(rest3, :fat_arrow),
         {:ok, body, rest5} <- parse_case_body(rest4) do
      span = BeamLang.Span.merge(case_tok.span, expr_span(body))
      match_case = %{pattern: pattern, guard: guard, body: body, span: span}

      case rest5 do
        [%Token{type: :comma} | rest6] -> parse_match_cases(rest6, [match_case | acc])
        _ -> parse_match_cases(rest5, [match_case | acc])
      end
    end
  end

  @spec parse_case_pattern([Token.t()]) ::
          {:ok, BeamLang.AST.pattern(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_case_pattern([%Token{type: :bang} | rest]) do
    with {:ok, tag_tok, rest1} <- expect(rest, :identifier) do
      case tag_tok.value do
        "ok" ->
          with {:ok, inner, rest2} <- parse_pattern(rest1) do
            case pattern_binding(inner) do
              {:ok, name, span} -> {:ok, {:res_ok_pat, %{name: name, span: span}}, rest2}
              {:error, err} -> {:error, err}
            end
          end

        "err" ->
          with {:ok, inner, rest2} <- parse_pattern(rest1) do
            case pattern_binding(inner) do
              {:ok, name, span} -> {:ok, {:res_err_pat, %{name: name, span: span}}, rest2}
              {:error, err} -> {:error, err}
            end
          end

        _ ->
          {:error, BeamLang.Error.new(:parser, "Invalid result case tag.", tag_tok.span)}
      end
    end
  end

  defp parse_case_pattern([%Token{type: :question} | rest]) do
    with {:ok, tag_tok, rest1} <- expect(rest, :identifier) do
      case tag_tok.value do
        "some" ->
          with {:ok, inner, rest2} <- parse_pattern(rest1) do
            case pattern_binding(inner) do
              {:ok, name, span} -> {:ok, {:opt_some_pat, %{name: name, span: span}}, rest2}
              {:error, err} -> {:error, err}
            end
          end

        "none" ->
          {:ok, {:opt_none_pat, %{span: tag_tok.span}}, rest1}

        _ ->
          {:error, BeamLang.Error.new(:parser, "Invalid optional case tag.", tag_tok.span)}
      end
    end
  end

  defp parse_case_pattern(tokens), do: parse_pattern(tokens)

  @spec parse_case_body([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_case_body([%Token{type: :lbrace} = lbrace | rest]) do
    with {:ok, block, rest1} <- parse_block(rest),
         {:ok, rbrace, rest2} <- expect(rest1, :rbrace) do
      span = BeamLang.Span.merge(lbrace.span, rbrace.span)
      {:ok, {:block_expr, %{block: block, span: span}}, rest2}
    end
  end

  defp parse_case_body(tokens) do
    parse_expression(tokens)
  end

  @spec parse_optional_guard([Token.t()]) ::
          {:ok, BeamLang.AST.expr() | nil, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_optional_guard([%Token{type: :if_kw} | rest]) do
    case parse_guard_expression(rest) do
      {:ok, guard, rest2} -> {:ok, guard, rest2}
      {:error, _} = error -> error
    end
  end

  defp parse_optional_guard(tokens), do: {:ok, nil, tokens}

  @spec parse_guard_expression([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_guard_expression(tokens) do
    with {:ok, left, rest1} <- parse_additive(tokens),
         {:ok, op, rest2} <- parse_compare_op(rest1),
         {:ok, right, rest3} <- parse_additive(rest2) do
      span = BeamLang.Span.merge(expr_span(left), expr_span(right))
      {:ok, {:binary, %{op: op, left: left, right: right, span: span}}, rest3}
    end
  end

  @spec parse_compare_op([Token.t()]) ::
          {:ok, BeamLang.AST.binary_op(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_compare_op([%Token{type: :eq_eq} | rest]), do: {:ok, :eq, rest}
  defp parse_compare_op([%Token{type: :neq} | rest]), do: {:ok, :neq, rest}
  defp parse_compare_op([%Token{type: :lt} | rest]), do: {:ok, :lt, rest}
  defp parse_compare_op([%Token{type: :gt} | rest]), do: {:ok, :gt, rest}
  defp parse_compare_op([%Token{type: :lte} | rest]), do: {:ok, :lte, rest}
  defp parse_compare_op([%Token{type: :gte} | rest]), do: {:ok, :gte, rest}
  defp parse_compare_op([%Token{} = tok | _]), do: {:error, error("Expected comparison operator.", tok)}

  @spec compare_op(atom()) :: BeamLang.AST.binary_op() | nil
  defp compare_op(:eq_eq), do: :eq
  defp compare_op(:neq), do: :neq
  defp compare_op(:lt), do: :lt
  defp compare_op(:gt), do: :gt
  defp compare_op(:lte), do: :lte
  defp compare_op(:gte), do: :gte
  defp compare_op(_other), do: nil

  @spec add_op(atom()) :: BeamLang.AST.binary_op() | nil
  defp add_op(:plus), do: :add
  defp add_op(:minus), do: :sub
  defp add_op(_other), do: nil

  @spec mul_op(atom()) :: BeamLang.AST.binary_op() | nil
  defp mul_op(:star), do: :mul
  defp mul_op(:slash), do: :div
  defp mul_op(:percent), do: :mod
  defp mul_op(_other), do: nil

  @spec parse_struct_literal([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_struct_literal([%Token{type: :lbrace} = lbrace | rest]) do
    with {:ok, fields, rest1} <- parse_struct_fields(rest, []),
         {:ok, rbrace, rest2} <- expect(rest1, :rbrace) do
      span = BeamLang.Span.merge(lbrace.span, rbrace.span)
      {:ok, {:struct, %{fields: fields, type: nil, span: span}}, rest2}
    end
  end

  defp parse_struct_literal([%Token{} = tok | _]) do
    {:error, error("Expected struct literal.", tok)}
  end

  @spec parse_struct_fields([Token.t()], [BeamLang.AST.field_assign()]) ::
          {:ok, [BeamLang.AST.field_assign()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_struct_fields([%Token{type: :rbrace} | _] = rest, acc) do
    {:ok, Enum.reverse(acc), rest}
  end

  defp parse_struct_fields(tokens, acc) do
    with {:ok, name_tok, rest1} <- expect(tokens, :identifier),
         {:ok, _eq, rest2} <- expect(rest1, :equals),
         {:ok, expr, rest3} <- parse_expression(rest2) do
      span = BeamLang.Span.merge(name_tok.span, expr_span(expr))
      field = %{name: name_tok.value, expr: expr, span: span}

      case rest3 do
        [%Token{type: :comma} | rest4] -> parse_struct_fields(rest4, [field | acc])
        _ -> parse_struct_fields(rest3, [field | acc])
      end
    end
  end

  @spec parse_literal([Token.t()]) ::
          {:ok, BeamLang.AST.literal(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_literal([%Token{type: :integer} = tok | rest]) do
    {:ok, {:integer, %{value: tok.value, span: tok.span}}, rest}
  end

  defp parse_literal([%Token{type: :float} = tok | rest]) do
    {:ok, {:float, %{value: tok.value, span: tok.span}}, rest}
  end

  defp parse_literal([%Token{type: :string} = tok | rest]) do
    if String.contains?(tok.value, "${") do
      with {:ok, parsed_string, expressions} <- parse_interpolations(tok.value, tok.span) do
        {:ok, {:interpolated_string, %{string: parsed_string, expressions: expressions, span: tok.span}}, rest}
      end
    else
      {:ok, {:string, %{value: tok.value, span: tok.span}}, rest}
    end
  end

  defp parse_literal([%Token{type: :char} = tok | rest]) do
    {:ok, {:char, %{value: tok.value, span: tok.span}}, rest}
  end

  @spec parse_literal([Token.t()]) ::
          {:ok, BeamLang.AST.literal(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_literal([%Token{type: :bool} = tok | rest]) do
    {:ok, {:bool, %{value: tok.value == "true", span: tok.span}}, rest}
  end

  @spec parse_literal([Token.t()]) ::
          {:ok, BeamLang.AST.literal(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_literal([%Token{} = tok | _]) do
    {:error, error("Expected literal.", tok)}
  end

  @spec parse_interpolations(binary(), BeamLang.Span.t()) ::
          {:ok, binary(), [BeamLang.AST.expr()]} | {:error, BeamLang.Error.t()}
  defp parse_interpolations(string, span) do
    case extract_interpolation_parts(string, [], []) do
      {:ok, parts, expressions} ->
        # Parse each expression string into an AST
        case parse_interpolation_expressions(expressions, span) do
          {:ok, parsed_exprs} ->
            # Reconstruct string with placeholders
            reconstructed = Enum.join(parts, "{}")
            {:ok, reconstructed, parsed_exprs}

          {:error, _} = err ->
            err
        end

      {:error, _} = err ->
        err
    end
  end

  @spec extract_interpolation_parts(binary(), [binary()], [binary()]) ::
          {:ok, [binary()], [binary()]} | {:error, BeamLang.Error.t()}
  defp extract_interpolation_parts("", parts, exprs) do
    {:ok, Enum.reverse(parts), Enum.reverse(exprs)}
  end

  defp extract_interpolation_parts(<<"${", rest::binary>>, parts, exprs) do
    case find_closing_brace(rest, 0, "") do
      {:ok, expr, remaining} ->
        extract_interpolation_parts(remaining, ["" | parts], [expr | exprs])

      {:error, _} = err ->
        err
    end
  end

  defp extract_interpolation_parts(<<char::utf8, rest::binary>>, [], exprs) do
    extract_interpolation_parts(rest, [<<char::utf8>> | []], exprs)
  end

  defp extract_interpolation_parts(<<char::utf8, rest::binary>>, [last | parts], exprs) do
    extract_interpolation_parts(rest, [last <> <<char::utf8>> | parts], exprs)
  end

  @spec find_closing_brace(binary(), non_neg_integer(), binary()) ::
          {:ok, binary(), binary()} | {:error, binary()}
  defp find_closing_brace("", _depth, _acc) do
    {:error, "Unclosed interpolation"}
  end

  defp find_closing_brace(<<"}", rest::binary>>, 0, acc) do
    {:ok, acc, rest}
  end

  defp find_closing_brace(<<"}", rest::binary>>, depth, acc) do
    find_closing_brace(rest, depth - 1, acc <> "}")
  end

  defp find_closing_brace(<<"{", rest::binary>>, depth, acc) do
    find_closing_brace(rest, depth + 1, acc <> "{")
  end

  defp find_closing_brace(<<char::utf8, rest::binary>>, depth, acc) do
    find_closing_brace(rest, depth, acc <> <<char::utf8>>)
  end

  @spec parse_interpolation_expressions([binary()], BeamLang.Span.t()) ::
          {:ok, [BeamLang.AST.expr()]} | {:error, BeamLang.Error.t()}
  defp parse_interpolation_expressions(exprs, _span) do
    results =
      Enum.map(exprs, fn expr_str ->
        case BeamLang.Lexer.tokenize(expr_str) do
          {:ok, tokens} ->
            case parse_expression(tokens) do
              {:ok, expr, _rest} -> {:ok, expr}
              {:error, _} = err -> err
            end

          {:error, _} = err ->
            err
        end
      end)

    # Check if all parsed successfully
    case Enum.find(results, fn r -> match?({:error, _}, r) end) do
      nil -> {:ok, Enum.map(results, fn {:ok, expr} -> expr end)}
      {:error, _} = err -> err
    end
  end

  @spec parse_pattern([Token.t()]) ::
          {:ok, BeamLang.AST.pattern(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_pattern([%Token{type: :identifier, value: "_"} = tok | rest]) do
    {:ok, {:wildcard, %{span: tok.span}}, rest}
  end

  defp parse_pattern([%Token{type: :identifier} = _tok, %Token{type: :lbrace} | _] = tokens) do
    parse_struct_pattern(tokens)
  end

  defp parse_pattern(tokens) do
    case parse_literal(tokens) do
      {:ok, _expr, _rest} = ok -> ok
      {:error, _} -> parse_pattern_identifier(tokens)
    end
  end

  @spec parse_pattern_identifier([Token.t()]) ::
          {:ok, BeamLang.AST.pattern(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_pattern_identifier([%Token{type: :identifier} = tok | rest]) do
    {:ok, {:pat_identifier, %{name: tok.value, span: tok.span}}, rest}
  end

  defp parse_pattern_identifier([%Token{} = tok | _]) do
    {:error, error("Expected pattern.", tok)}
  end

  @spec parse_struct_pattern([Token.t()]) ::
          {:ok, BeamLang.AST.pattern(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_struct_pattern(tokens) do
    with {:ok, {name, name_span}, rest1} <- parse_qualified_identifier(tokens),
         {:ok, _lbrace, rest2} <- expect(rest1, :lbrace),
         {:ok, fields, rest3} <- parse_struct_pattern_fields(rest2, []),
         {:ok, rbrace, rest4} <- expect(rest3, :rbrace) do
      span = BeamLang.Span.merge(name_span, rbrace.span)
      {:ok, {:struct_pattern, %{name: name, fields: fields, span: span}}, rest4}
    end
  end

  @spec parse_struct_pattern_fields([Token.t()], [BeamLang.AST.pattern_field()]) ::
          {:ok, [BeamLang.AST.pattern_field()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_struct_pattern_fields([%Token{type: :rbrace} | _] = rest, acc) do
    {:ok, Enum.reverse(acc), rest}
  end

  defp parse_struct_pattern_fields(tokens, acc) do
    with {:ok, name_tok, rest1} <- expect(tokens, :identifier) do
      case rest1 do
        [%Token{type: :equals} | rest2] ->
          with {:ok, pattern, rest3} <- parse_pattern(rest2) do
            span = BeamLang.Span.merge(name_tok.span, pattern_span(pattern))
            field = %{name: name_tok.value, pattern: pattern, span: span}
            case rest3 do
              [%Token{type: :comma} | rest4] -> parse_struct_pattern_fields(rest4, [field | acc])
              _ -> parse_struct_pattern_fields(rest3, [field | acc])
            end
          end

        _ ->
          pattern = {:pat_identifier, %{name: name_tok.value, span: name_tok.span}}
          field = %{name: name_tok.value, pattern: pattern, span: name_tok.span}
          case rest1 do
            [%Token{type: :comma} | rest2] -> parse_struct_pattern_fields(rest2, [field | acc])
            _ -> parse_struct_pattern_fields(rest1, [field | acc])
          end
      end
    end
  end

  @spec parse_identifier([Token.t()]) ::
          {:ok, BeamLang.AST.expr(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_identifier([%Token{type: :identifier} = tok | rest]) do
    {:ok, {:identifier, %{name: tok.value, span: tok.span}}, rest}
  end

  defp parse_identifier([%Token{} = tok | _]) do
    {:error, error("Expected identifier.", tok)}
  end

  @spec expect([Token.t()], atom()) :: {:ok, Token.t(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp expect([%Token{type: type} = tok | rest], type) do
    {:ok, tok, rest}
  end

  @spec expect([Token.t()], atom()) :: {:ok, Token.t(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp expect([%Token{} = tok | _rest], type) do
    {:error, error("Expected #{type}.", tok)}
  end

  @spec expect([Token.t()], atom()) :: {:ok, Token.t(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp expect([], type) do
    {:error, BeamLang.Error.new(:parser, "Expected #{type}, got end of input.", eof_span("<source>"))}
  end

  @spec unexpected([Token.t()]) :: BeamLang.Error.t()
  defp unexpected([%Token{} = tok | _]) do
    BeamLang.Error.new(:parser, "Unexpected tokens after end of function.", tok.span)
  end

  @spec unexpected([Token.t()]) :: BeamLang.Error.t()
  defp unexpected([]) do
    BeamLang.Error.new(:parser, "Unexpected end of input.", eof_span("<source>"))
  end

  @spec error(binary(), Token.t()) :: BeamLang.Error.t()
  defp error(message, %Token{} = tok) do
    BeamLang.Error.new(:parser, message, tok.span)
  end

  @spec expr_span(BeamLang.AST.expr()) :: BeamLang.Span.t()
  defp expr_span({:integer, %{span: span}}), do: span
  defp expr_span({:float, %{span: span}}), do: span
  defp expr_span({:string, %{span: span}}), do: span
  defp expr_span({:interpolated_string, %{span: span}}), do: span
  defp expr_span({:char, %{span: span}}), do: span
  defp expr_span({:bool, %{span: span}}), do: span
  defp expr_span({:identifier, %{span: span}}), do: span
  defp expr_span({:call, %{span: span}}), do: span
  defp expr_span({:struct, %{span: span}}), do: span
  defp expr_span({:field, %{span: span}}), do: span
  defp expr_span({:block_expr, %{span: span}}), do: span
  defp expr_span({:match, %{span: span}}), do: span
  defp expr_span({:binary, %{span: span}}), do: span
  defp expr_span({:if_expr, %{span: span}}), do: span
  defp expr_span({:opt_some, %{span: span}}), do: span
  defp expr_span({:opt_none, %{span: span}}), do: span
  defp expr_span({:res_ok, %{span: span}}), do: span
  defp expr_span({:res_err, %{span: span}}), do: span
  defp expr_span({:lambda, %{span: span}}), do: span
  defp expr_span({:method_call, %{span: span}}), do: span
  defp expr_span({:list_literal, %{span: span}}), do: span

  @spec pattern_span(BeamLang.AST.pattern()) :: BeamLang.Span.t()
  defp pattern_span({:integer, %{span: span}}), do: span
  defp pattern_span({:float, %{span: span}}), do: span
  defp pattern_span({:string, %{span: span}}), do: span
  defp pattern_span({:char, %{span: span}}), do: span
  defp pattern_span({:bool, %{span: span}}), do: span
  defp pattern_span({:wildcard, %{span: span}}), do: span
  defp pattern_span({:pat_identifier, %{span: span}}), do: span
  defp pattern_span({:struct_pattern, %{span: span}}), do: span
  defp pattern_span({:opt_some_pat, %{span: span}}), do: span
  defp pattern_span({:opt_none_pat, %{span: span}}), do: span
  defp pattern_span({:res_ok_pat, %{span: span}}), do: span
  defp pattern_span({:res_err_pat, %{span: span}}), do: span

  @spec pattern_binding(BeamLang.AST.pattern()) ::
          {:ok, binary(), BeamLang.Span.t()} | {:error, BeamLang.Error.t()}
  defp pattern_binding({:pat_identifier, %{name: name, span: span}}), do: {:ok, name, span}
  defp pattern_binding({:wildcard, %{span: span}}), do: {:ok, "_", span}
  defp pattern_binding(other) do
    {:error, BeamLang.Error.new(:parser, "Expected identifier in tagged pattern.", pattern_span(other))}
  end

  @spec semicolon_span([Token.t()], [Token.t()]) :: BeamLang.Span.t()
  defp semicolon_span([%Token{type: :semicolon} = tok | _], _rest), do: tok.span
  defp semicolon_span(_before, [%Token{type: :semicolon} = tok | _]), do: tok.span

  @spec block_span([BeamLang.AST.stmt()]) :: BeamLang.Span.t()
  defp block_span([]), do: BeamLang.Span.new("<source>", 0, 0)

  defp block_span([first | _] = stmts) do
    first_span = stmt_span(first)
    last_span = stmt_span(List.last(stmts))
    BeamLang.Span.merge(first_span, last_span)
  end

  @spec block_span(BeamLang.AST.block()) :: BeamLang.Span.t()
  defp block_span({:block, %{stmts: stmts}}), do: block_span(stmts)

  @spec program_span([BeamLang.AST.import()], [BeamLang.AST.type_def()], [BeamLang.AST.func()]) :: BeamLang.Span.t()
  defp program_span(imports, types, funcs) do
    spans =
      (imports ++ types ++ funcs)
      |> Enum.map(fn
        {:import, %{span: span}} -> span
        {:type_def, %{span: span}} -> span
        {:function, %{span: span}} -> span
      end)

    case spans do
      [first | _] -> BeamLang.Span.merge(first, List.last(spans))
      [] -> BeamLang.Span.new("<source>", 0, 0)
    end
  end


  @spec stmt_span(BeamLang.AST.stmt()) :: BeamLang.Span.t()
  defp stmt_span({:return, %{span: span}}), do: span
  defp stmt_span({:expr, %{span: span}}), do: span
  defp stmt_span({:let, %{span: span}}), do: span
  defp stmt_span({:assign, %{span: span}}), do: span
  defp stmt_span({:guard, %{span: span}}), do: span
  defp stmt_span({:if_stmt, %{span: span}}), do: span
  defp stmt_span({:while, %{span: span}}), do: span
  defp stmt_span({:loop, %{span: span}}), do: span
  defp stmt_span({:for, %{span: span}}), do: span
  defp stmt_span({:break, %{span: span}}), do: span

  @spec branch_span(BeamLang.AST.block(), BeamLang.AST.if_else_branch() | nil) :: BeamLang.Span.t()
  defp branch_span(then_block, nil), do: block_span(then_block)

  defp branch_span(then_block, {:else_block, %{block: else_block}}) do
    BeamLang.Span.merge(block_span(then_block), block_span(else_block))
  end

  defp branch_span(then_block, {:else_if, %{if: if_stmt}}) do
    BeamLang.Span.merge(block_span(then_block), stmt_span(if_stmt))
  end

  @spec rbrace_span([Token.t()], [Token.t()]) :: BeamLang.Span.t()
  defp rbrace_span(_before, [%Token{type: :rbrace} = tok | _]), do: tok.span
  defp rbrace_span([%Token{type: :rbrace} = tok | _], _rest), do: tok.span

  @spec eof_span(binary()) :: BeamLang.Span.t()
  defp eof_span(file_id) do
    BeamLang.Span.new(file_id, 0, 0)
  end
  @spec parse_type_def([Token.t()], boolean()) ::
          {:ok, BeamLang.AST.type_def(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_type_def(tokens, exported) do
    with {:ok, type_tok, rest1} <- expect(tokens, :type_kw),
         {:ok, name_tok, rest2} <- parse_type_def_name(rest1),
         {:ok, {params, params_span}, rest3} <- parse_type_params(rest2),
         {:ok, _lbrace, rest4} <- expect(rest3, :lbrace),
         {:ok, fields, rest5} <- parse_type_fields(rest4, []),
         {:ok, _rbrace, rest6} <- expect(rest5, :rbrace) do
      span = BeamLang.Span.merge(type_tok.span, name_tok.span)
      span =
        case params_span do
          nil -> span
          _ -> BeamLang.Span.merge(span, params_span)
        end
      span = BeamLang.Span.merge(span, rbrace_span(rest5, rest6))
      {:ok, {:type_def, %{name: name_tok.value, params: params, fields: fields, exported: exported, span: span}}, rest6}
    end
  end

  @spec parse_error_def([Token.t()], boolean()) ::
          {:ok, BeamLang.AST.error_def(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_error_def(tokens, exported) do
    with {:ok, error_tok, rest1} <- expect(tokens, :error_kw),
         {:ok, name_tok, rest2} <- parse_type_def_name(rest1),
         {:ok, _lbrace, rest3} <- expect(rest2, :lbrace),
         {:ok, fields, rest4} <- parse_error_fields(rest3, []),
         {:ok, _rbrace, rest5} <- expect(rest4, :rbrace) do
      span = BeamLang.Span.merge(error_tok.span, rbrace_span(rest4, rest5))
      {:ok, {:error_def, %{name: name_tok.value, fields: fields, exported: exported, span: span}}, rest5}
    end
  end

  @spec parse_error_fields([Token.t()], [BeamLang.AST.field_def()]) ::
          {:ok, [BeamLang.AST.field_def()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_error_fields([%Token{type: :rbrace} | _] = rest, acc) do
    {:ok, Enum.reverse(acc), rest}
  end

  defp parse_error_fields([%Token{type: :identifier} = name_tok | rest], acc) do
    with {:ok, _colon, rest1} <- expect(rest, :colon),
         {:ok, {type, type_span}, rest2} <- parse_type_name(rest1) do
      span = BeamLang.Span.merge(name_tok.span, type_span)
      field = %{name: name_tok.value, type: type, internal: false, span: span}

      case rest2 do
        [%Token{type: :comma} | rest3] -> parse_error_fields(rest3, [field | acc])
        _ -> parse_error_fields(rest2, [field | acc])
      end
    end
  end

  defp parse_error_fields([%Token{} = tok | _], _acc) do
    {:error, error("Expected field name or '}'.", tok)}
  end

  defp parse_type_def_name([%Token{type: :identifier} = tok | rest]), do: {:ok, tok, rest}
  defp parse_type_def_name([%Token{type: :type, value: "String"} = tok | rest]), do: {:ok, tok, rest}
  defp parse_type_def_name([%Token{} = tok | _]), do: {:error, error("Expected identifier.", tok)}

  @spec parse_type_params([Token.t()]) ::
          {:ok, {[binary()], BeamLang.Span.t() | nil}, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_type_params([%Token{type: :lt} = lt_tok | rest]) do
    with {:ok, params, rest1} <- parse_type_param_list(rest, []),
         {:ok, gt_tok, rest2} <- expect(rest1, :gt) do
      span = BeamLang.Span.merge(lt_tok.span, gt_tok.span)
      {:ok, {params, span}, rest2}
    end
  end

  defp parse_type_params(tokens), do: {:ok, {[], nil}, tokens}

  @spec parse_type_param_list([Token.t()], [binary()]) ::
          {:ok, [binary()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_type_param_list([%Token{type: :identifier} = tok | rest], acc) do
    case rest do
      [%Token{type: :comma} | rest2] -> parse_type_param_list(rest2, [tok.value | acc])
      _ -> {:ok, Enum.reverse([tok.value | acc]), rest}
    end
  end

  defp parse_type_param_list([%Token{} = tok | _], _acc) do
    {:error, error("Expected type parameter.", tok)}
  end

  @spec parse_type_fields([Token.t()], [BeamLang.AST.field_def()]) ::
          {:ok, [BeamLang.AST.field_def()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_type_fields([%Token{type: :rbrace} | _] = rest, acc) do
    {:ok, Enum.reverse(acc), rest}
  end

  defp parse_type_fields([%Token{type: :internal_kw} | rest], acc) do
    with {:ok, name_tok, rest1} <- expect(rest, :identifier),
         {:ok, _colon, rest2} <- expect(rest1, :colon),
         {:ok, {type_name, type_span}, rest3} <- parse_type_name(rest2) do
      span = BeamLang.Span.merge(name_tok.span, type_span)
      field = %{name: name_tok.value, type: type_name, internal: true, span: span}

      case rest3 do
        [%Token{type: :comma} | rest4] -> parse_type_fields(rest4, [field | acc])
        _ -> parse_type_fields(rest3, [field | acc])
      end
    end
  end

  defp parse_type_fields(tokens, acc) do
    with {:ok, name_tok, rest1} <- expect(tokens, :identifier),
         {:ok, _colon, rest2} <- expect(rest1, :colon),
         {:ok, {type_name, type_span}, rest3} <- parse_type_name(rest2) do
      span = BeamLang.Span.merge(name_tok.span, type_span)
      field = %{name: name_tok.value, type: type_name, internal: false, span: span}

      case rest3 do
        [%Token{type: :comma} | rest4] -> parse_type_fields(rest4, [field | acc])
        _ -> parse_type_fields(rest3, [field | acc])
      end
    end
  end

  @spec parse_type_name([Token.t()]) ::
          {:ok, BeamLang.AST.type_name(), [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_type_name(tokens) do
    with {:ok, {base_type, base_span}, rest} <- parse_base_type(tokens) do
      parse_type_suffix(base_type, base_span, rest)
    end
  end

  @spec parse_base_type([Token.t()]) ::
          {:ok, {BeamLang.AST.type_name(), BeamLang.Span.t()}, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_base_type([%Token{type: :type} = tok | rest]) do
    {:ok, {String.to_atom(tok.value), tok.span}, rest}
  end

  defp parse_base_type([%Token{type: :lbracket} = lbracket_tok | rest]) do
    with {:ok, {elem_type, _elem_span}, rest1} <- parse_type_name(rest),
         {:ok, rbracket_tok, rest2} <- expect(rest1, :rbracket) do
      span = BeamLang.Span.merge(lbracket_tok.span, rbracket_tok.span)
      {:ok, {{:generic, {:named, "List"}, [elem_type]}, span}, rest2}
    end
  end

  defp parse_base_type([%Token{type: :fn} = fn_tok | rest]) do
    with {:ok, _lparen, rest1} <- expect(rest, :lparen),
         {:ok, params, rest2} <- parse_type_params(rest1, []),
         {:ok, _rparen, rest3} <- expect(rest2, :rparen),
         {:ok, _arrow, rest4} <- expect(rest3, :arrow),
         {:ok, {return_type, return_span}, rest5} <- parse_type_name(rest4) do
      span = BeamLang.Span.merge(fn_tok.span, return_span)
      {:ok, {{:fn, params, return_type}, span}, rest5}
    end
  end

  defp parse_base_type(tokens) do
    case parse_qualified_identifier(tokens) do
      {:ok, {name, span}, rest} -> {:ok, {{:named, name}, span}, rest}
      {:error, _} = error -> error
    end
  end

  @spec parse_type_suffix(BeamLang.AST.type_name(), BeamLang.Span.t(), [Token.t()]) ::
          {:ok, {BeamLang.AST.type_name(), BeamLang.Span.t()}, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_type_suffix(type, span, [%Token{type: :lt} = lt_tok | rest]) do
    with {:ok, args, rest1} <- parse_type_args(rest, []),
         {:ok, gt_tok, rest2} <- expect(rest1, :gt) do
      span = BeamLang.Span.merge(span, lt_tok.span)
      span = BeamLang.Span.merge(span, gt_tok.span)
      parse_type_suffix({:generic, type, args}, span, rest2)
    end
  end

  defp parse_type_suffix(type, span, [%Token{type: :question} = tok | rest]) do
    span = BeamLang.Span.merge(span, tok.span)
    parse_type_suffix({:optional, type}, span, rest)
  end

  defp parse_type_suffix(type, span, [%Token{type: :bang} = tok | rest]) do
    with {:ok, {err_type, err_span}, rest2} <- parse_type_name(rest) do
      span = BeamLang.Span.merge(span, tok.span)
      span = BeamLang.Span.merge(span, err_span)
      parse_type_suffix({:result, type, err_type}, span, rest2)
    end
  end

  defp parse_type_suffix(type, span, rest), do: {:ok, {type, span}, rest}

  @spec parse_qualified_identifier([Token.t()]) ::
          {:ok, {binary(), BeamLang.Span.t()}, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_qualified_identifier([%Token{type: :identifier} = first | rest]) do
    case rest do
      [%Token{type: :double_colon}, %Token{type: :identifier} = second | rest2] ->
        span = BeamLang.Span.merge(first.span, second.span)
        {:ok, {"#{first.value}::#{second.value}", span}, rest2}

      _ ->
        {:ok, {first.value, first.span}, rest}
    end
  end

  defp parse_qualified_identifier([%Token{} = tok | _]) do
    {:error, error("Expected identifier.", tok)}
  end

  @spec parse_type_args([Token.t()], [BeamLang.AST.type_name()]) ::
          {:ok, [BeamLang.AST.type_name()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_type_args(tokens, acc) do
    with {:ok, {type_name, _span}, rest1} <- parse_type_name(tokens) do
      case rest1 do
        [%Token{type: :comma} | rest2] -> parse_type_args(rest2, [type_name | acc])
        _ -> {:ok, Enum.reverse([type_name | acc]), rest1}
      end
    end
  end

  @spec parse_type_params([Token.t()], [BeamLang.AST.type_name()]) ::
          {:ok, [BeamLang.AST.type_name()], [Token.t()]} | {:error, BeamLang.Error.t()}
  defp parse_type_params([%Token{type: :rparen} | _] = rest, acc) do
    {:ok, Enum.reverse(acc), rest}
  end

  defp parse_type_params(tokens, acc) do
    with {:ok, {type_name, _span}, rest1} <- parse_type_name(tokens) do
      case rest1 do
        [%Token{type: :comma} | rest2] -> parse_type_params(rest2, [type_name | acc])
        _ -> {:ok, Enum.reverse([type_name | acc]), rest1}
      end
    end
  end

  @spec parse_optional_type([Token.t()]) :: {BeamLang.AST.type_name() | nil, [Token.t()]}
  defp parse_optional_type([%Token{type: :colon} | rest]) do
    case parse_type_name(rest) do
      {:ok, {type_name, _span}, rest2} -> {type_name, rest2}
      {:error, _} -> {nil, rest}
    end
  end

  defp parse_optional_type(tokens), do: {nil, tokens}

end
