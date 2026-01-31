defmodule BeamLang.Codegen do
  @moduledoc """
  Generates Erlang abstract forms from BeamLang AST.
  """

  @module_name :beamlang_program

  @spec to_erlang_forms(BeamLang.AST.t()) :: list()
  def to_erlang_forms({:program, %{module: module, functions: functions}}) when is_list(functions) do
    line = 1

    externals = externals_map(functions)
    functions_map = functions_map(functions)
    function_forms =
      functions
      |> Enum.filter(&function_has_body?/1)
      |> Enum.map(&function_form(&1, externals, functions_map))
    module_atom = module_atom(module)

    [
      {:attribute, line, :module, module_atom},
      {:attribute, line, :export, exports(functions)}
      | function_forms
    ]
  end

  @spec exports([BeamLang.AST.func()]) :: list()
  defp exports(functions) do
    functions
    |> Enum.filter(&function_has_body?/1)
    |> Enum.filter(fn {:function, %{name: name, exported: exported, internal: internal}} -> name == "main" or (exported and not internal) end)
    |> Enum.map(fn {:function, %{name: name, params: params}} ->
      {String.to_atom(name), length(params)}
    end)
  end

  @spec function_form(BeamLang.AST.func(), map(), map()) :: tuple()
  defp function_form({:function, %{name: name, params: params, body: {:block, %{stmts: stmts}}}}, externals, functions_map) do
    line = 1
    {params_form, env, counter} = params_form(params, line, %{}, 0)
    env = Map.put(env, :__externals__, externals)
    env = Map.put(env, :__functions__, functions_map)
    {expr, _env, counter} = stmt_expr_tree(stmts, env, counter)
    {expr, _counter} = unwrap_return(expr, counter)

    {:function, line, String.to_atom(name), length(params),
     [return_clause(line, params_form, [expr])]}
  end

  defp function_has_body?({:function, %{body: body}}), do: body != nil

  defp externals_map(functions) do
    names_with_body =
      functions
      |> Enum.filter(&function_has_body?/1)
      |> Enum.map(fn {:function, %{name: name}} -> name end)
      |> MapSet.new()

    functions
    |> Enum.reduce(%{}, fn {:function, %{name: name, external: external}}, acc ->
      cond do
        external == nil -> acc
        MapSet.member?(names_with_body, name) -> acc
        true -> Map.put(acc, name, external)
      end
    end)
  end

  defp functions_map(functions) do
    functions
    |> Enum.filter(&function_has_body?/1)
    |> Enum.reduce(%{}, fn {:function, %{name: name, params: params}}, acc ->
      Map.put(acc, name, length(params))
    end)
  end

  @spec stmt_expr_tree([BeamLang.AST.stmt()], map(), non_neg_integer()) ::
          {tuple(), map(), non_neg_integer()}
  defp stmt_expr_tree([], env, counter) do
    {{:atom, 1, :ok}, env, counter}
  end

  defp stmt_expr_tree([stmt], env, counter) do
    case stmt do
      {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}} ->
        {expr, env, counter} = if_stmt_expr(cond, then_block, else_branch, env, counter, {:atom, 1, :ok})
        {expr, env, counter}

      {:while, %{cond: cond, body: {:block, %{stmts: body_stmts}}}} ->
        {expr, env, counter} = while_expr(cond, body_stmts, env, counter)
        {expr, env, counter}

      {:loop, %{body: {:block, %{stmts: body_stmts}}}} ->
        {expr, env, counter} = loop_expr(body_stmts, env, counter)
        {expr, env, counter}

      {:for, %{name: name, collection: collection, body: {:block, %{stmts: body_stmts}}} = info} ->
        collection_type = Map.get(info, :collection_type, :any)
        {expr, env, counter} = for_expr(name, collection, body_stmts, env, counter, collection_type)
        {expr, env, counter}

      {:guard, %{cond: cond, else_block: {:block, %{stmts: else_stmts}}}} ->
        cond_form = expr_form(1, cond, env)
        {else_expr, _else_env, counter} = stmt_expr_tree(else_stmts, env, counter)
        rest_expr = {:atom, 1, :ok}

        clause_true = {:clause, 1, [{:atom, 1, true}], [], [rest_expr]}
        clause_false = {:clause, 1, [{:atom, 1, false}], [], [else_expr]}

        {{:case, 1, cond_form, [clause_true, clause_false]}, env, counter}

      _ ->
        stmt_form(stmt, env, counter)
    end
  end

  defp stmt_expr_tree([stmt | rest], env, counter) do
    case stmt do
      {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}} ->
        {rest_expr, env, counter} = stmt_expr_tree(rest, env, counter)
        {expr, env, counter} = if_stmt_expr(cond, then_block, else_branch, env, counter, rest_expr)
        {expr, env, counter}

      {:while, %{cond: cond, body: {:block, %{stmts: body_stmts}}}} ->
        {loop_expr, env, counter} = while_expr(cond, body_stmts, env, counter)
        {rest_expr, env, counter} = stmt_expr_tree(rest, env, counter)
        {expr, counter} = sequence_expr(loop_expr, rest_expr, counter)
        {expr, env, counter}

      {:loop, %{body: {:block, %{stmts: body_stmts}}}} ->
        {loop_expr, env, counter} = loop_expr(body_stmts, env, counter)
        {rest_expr, env, counter} = stmt_expr_tree(rest, env, counter)
        {expr, counter} = sequence_expr(loop_expr, rest_expr, counter)
        {expr, env, counter}

      {:for, %{name: name, collection: collection, body: {:block, %{stmts: body_stmts}}} = info} ->
        collection_type = Map.get(info, :collection_type, :any)
        {for_expr, env, counter} = for_expr(name, collection, body_stmts, env, counter, collection_type)
        {rest_expr, env, counter} = stmt_expr_tree(rest, env, counter)
        {expr, counter} = sequence_expr(for_expr, rest_expr, counter)
        {expr, env, counter}

      {:guard, %{cond: cond, else_block: {:block, %{stmts: else_stmts}}}} ->
        cond_form = expr_form(1, cond, env)
        {else_expr, _else_env, counter} = stmt_expr_tree(else_stmts, env, counter)
        {rest_expr, env, counter} = stmt_expr_tree(rest, env, counter)

        clause_true = {:clause, 1, [{:atom, 1, true}], [], [rest_expr]}
        clause_false = {:clause, 1, [{:atom, 1, false}], [], [else_expr]}

        {{:case, 1, cond_form, [clause_true, clause_false]}, env, counter}

      _ ->
        {form, env, counter} = stmt_form(stmt, env, counter)
        {rest_expr, env, counter} = stmt_expr_tree(rest, env, counter)
        {expr, counter} = sequence_expr(form, rest_expr, counter)
        {expr, env, counter}
    end
  end

  @spec stmt_form(BeamLang.AST.stmt(), map(), non_neg_integer()) ::
          {tuple(), map(), non_neg_integer()}
  defp stmt_form({:let, %{name: name, expr: expr}}, env, counter) do
    {var, counter} = fresh_var(name, counter)
    form = match_form(1, var, expr, env)
    {form, Map.put(env, name, var), counter}
  end

  defp stmt_form({:assign, %{target: target, expr: expr}}, env, counter) do
    case assignment_form(1, target, expr, env, counter) do
      {:ok, form, new_env, new_counter} -> {form, new_env, new_counter}
      {:error, _} -> {{:atom, 1, :ok}, env, counter}
    end
  end

  defp stmt_form({:expr, %{expr: expr}}, env, counter) do
    {expr_form(1, expr, env), env, counter}
  end

  defp stmt_form({:return, %{expr: nil}}, env, counter) do
    {return_marker({:atom, 1, :ok}), env, counter}
  end

  defp stmt_form({:return, %{expr: expr}}, env, counter) do
    {return_marker(expr_form(1, expr, env)), env, counter}
  end

  defp stmt_form({:break, %{}}, env, counter) do
    {{:atom, 1, :__break__}, env, counter}
  end

  @spec return_clause(non_neg_integer(), [tuple()], [tuple()]) :: tuple()
  defp return_clause(line, params, exprs) do
    {:clause, line, params, [], exprs}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:integer, %{value: value}}, _env) do
    {:integer, line, value}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:float, %{value: value}}, _env) do
    {:float, line, value}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:string, %{value: value}}, _env) do
    data = {:string, line, String.to_charlist(value)}
    {:call, line, {:atom, line, :string_new}, [data]}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:char, %{value: value}}, _env) do
    {:tuple, line, [{:atom, line, :char}, {:integer, line, value}]}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:struct, %{fields: fields, type: type}}, env) do
    entries =
      Enum.map(fields, fn %{name: name, expr: expr} ->
        {:map_field_assoc, line, {:atom, line, String.to_atom(name)}, expr_form(line, expr, env)}
      end)

    entries =
      case type do
        nil ->
          entries

        _ ->
          type_label = type_label(type)
          [
            {:map_field_assoc, line, {:atom, line, :__beamlang_type__},
             {:string, line, String.to_charlist(type_label)}}
            | entries
          ]
      end

    {:map, line, entries}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:field, %{target: target, name: name}}, env) do
    {:call, line, {:remote, line, {:atom, line, :maps}, {:atom, line, :get}},
     [{:atom, line, String.to_atom(name)}, expr_form(line, target, env)]}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(_line, {:block_expr, %{block: {:block, %{stmts: stmts}}}}, env) do
    block_expr_form({:block, %{stmts: stmts}}, env)
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:match, %{expr: expr, cases: cases}}, env) do
    clauses = Enum.map(cases, &case_clause_form(line, &1, env))
    {:case, line, expr_form(line, expr, env), clauses}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch}}, env) do
    cond_form = expr_form(line, cond, env)
    then_expr = block_expr_form(then_block, env)
    else_expr = else_branch_expr(else_branch, env)

    clause_true = {:clause, line, [{:atom, line, true}], [], [then_expr]}
    clause_false = {:clause, line, [{:atom, line, false}], [], [else_expr]}
    {:case, line, cond_form, [clause_true, clause_false]}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:bool, %{value: value}}, _env) when is_boolean(value) do
    {:atom, line, value}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:call, %{name: name, args: args}}, env) do
    case external_call(name, args, line, env) do
      {:ok, form} ->
        form

      :error ->
        if Map.has_key?(env, name) do
          {:call, line, {:var, line, Map.get(env, name)}, Enum.map(args, &expr_form(line, &1, env))}
        else
          case qualified_name(name) do
            {:ok, mod, fun} ->
              {:call, line, {:remote, line, {:atom, line, String.to_atom(mod)}, {:atom, line, String.to_atom(fun)}},
               Enum.map(args, &expr_form(line, &1, env))}

            :error ->
              {:call, line, {:atom, line, String.to_atom(name)},
               Enum.map(args, &expr_form(line, &1, env))}
          end
        end
    end
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:identifier, %{name: name}}, env) do
    functions_map = Map.get(env, :__functions__, %{})

    cond do
      Map.has_key?(env, name) ->
        {:var, line, Map.get(env, name)}

      Map.has_key?(functions_map, name) ->
        arity = Map.fetch!(functions_map, name)
        {:fun, line, {:function, String.to_atom(name), arity}}

      true ->
        {:var, line, var_atom(name)}
    end
  end

  defp expr_form(line, {:lambda, %{params: params, body: body}}, env) do
    {params_form, params_env} = lambda_params_form(params, line)
    env = Map.merge(env, params_env)
    {expr, _env, counter} = stmt_expr_tree(block_stmts(body), env, 0)
    {expr, _counter} = unwrap_return(expr, counter)
    clause = {:clause, line, params_form, [], [expr]}
    {:fun, line, {:clauses, [clause]}}
  end

  defp expr_form(line, {:method_call, %{target: target, name: name, args: args}}, env) do
    fun_expr = expr_form(line, {:field, %{target: target, name: name}}, env)
    args = [expr_form(line, target, env) | Enum.map(args, &expr_form(line, &1, env))]
    {:call, line, fun_expr, args}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:binary, %{op: op, left: left, right: right}}, env) do
    {:op, line, op_atom(op), expr_form(line, left, env), expr_form(line, right, env)}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:opt_some, %{expr: expr}}, env) do
    value = expr_form(line, expr, env)
    {:call, line, {:atom, line, :optional_some}, [value]}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:opt_none, %{}}, _env) do
    {:call, line, {:atom, line, :optional_none}, []}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:res_ok, %{expr: expr}}, env) do
    value = expr_form(line, expr, env)
    {:call, line, {:atom, line, :result_ok}, [value]}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:res_err, %{expr: expr}}, env) do
    value = expr_form(line, expr, env)
    {:call, line, {:atom, line, :result_err}, [value]}
  end

  @spec assignment_form(
          non_neg_integer(),
          BeamLang.AST.expr(),
          BeamLang.AST.expr(),
          map(),
          non_neg_integer()
        ) ::
          {:ok, tuple(), map(), non_neg_integer()} | {:error, atom()}
  defp assignment_form(line, {:identifier, %{name: name}}, expr, env, counter) do
    {var, counter} = fresh_var(name, counter)
    form = match_form(line, var, expr, env)
    {:ok, form, Map.put(env, name, var), counter}
  end

  defp assignment_form(
         line,
         {:field, %{target: {:identifier, %{name: name}}, name: field}},
         expr,
         env,
         counter
       ) do
    base_var = Map.get(env, name, var_atom(name))

    updated_map =
      {:call, line, {:remote, line, {:atom, line, :maps}, {:atom, line, :put}},
       [{:atom, line, String.to_atom(field)}, expr_form(line, expr, env), {:var, line, base_var}]}

    {new_var, counter} = fresh_var(name, counter)
    form = {:match, line, {:var, line, new_var}, updated_map}
    {:ok, form, Map.put(env, name, new_var), counter}
  end

  defp assignment_form(_line, _target, _expr, _env, _counter), do: {:error, :invalid_target}

  @spec match_form(non_neg_integer(), atom(), BeamLang.AST.expr(), map()) :: tuple()
  defp match_form(line, var, expr, env) do
    {:match, line, {:var, line, var}, expr_form(line, expr, env)}
  end

  @spec if_stmt_expr(BeamLang.AST.expr(), BeamLang.AST.block(), BeamLang.AST.if_else_branch() | nil, map(), non_neg_integer(), tuple()) ::
          {tuple(), map(), non_neg_integer()}
  defp if_stmt_expr(cond, then_block, else_branch, env, counter, rest_expr) do
    cond_form = expr_form(1, cond, env)
    then_expr = block_expr_form(then_block, env)
    else_expr =
      case else_branch do
        nil -> rest_expr
        _ -> else_branch_expr(else_branch, env)
      end

    {then_seq, counter} = sequence_expr(then_expr, rest_expr, counter)

    {else_seq, counter} =
      if else_branch == nil do
        {rest_expr, counter}
      else
        sequence_expr(else_expr, rest_expr, counter)
      end

    clause_true = {:clause, 1, [{:atom, 1, true}], [], [then_seq]}
    clause_false = {:clause, 1, [{:atom, 1, false}], [], [else_seq]}
    {{:case, 1, cond_form, [clause_true, clause_false]}, env, counter}
  end

  @spec while_expr(BeamLang.AST.expr(), [BeamLang.AST.stmt()], map(), non_neg_integer()) ::
          {tuple(), map(), non_neg_integer()}
  defp while_expr(cond, body_stmts, env, counter) do
    loop_fun_expr(cond, body_stmts, env, counter, :while)
  end

  @spec loop_expr([BeamLang.AST.stmt()], map(), non_neg_integer()) ::
          {tuple(), map(), non_neg_integer()}
  defp loop_expr(body_stmts, env, counter) do
    loop_fun_expr(:always_true, body_stmts, env, counter, :loop)
  end

  @spec for_expr(binary(), BeamLang.AST.expr(), [BeamLang.AST.stmt()], map(), non_neg_integer(), BeamLang.AST.type_name()) ::
          {tuple(), map(), non_neg_integer()}
  defp for_expr(name, collection, body_stmts, env, counter, collection_type) do
    line = 1
    {fun_var, counter} = fresh_var("for_loop", counter)
    fun_name = fun_var
    {item_var, counter} = fresh_var(name, counter)
    {rest_var, counter} = fresh_var("rest", counter)
    {head_var, counter} = {item_var, counter}

    body_env = Map.put(env, name, item_var)
    {body_expr, _env_body, counter} = stmt_expr_tree(body_stmts, body_env, counter)
    continue_call = {:call, line, {:var, line, fun_name}, [{:var, line, rest_var}]}
    {body_case, counter} = break_case(body_expr, continue_call, counter)

    clause_nil = {:clause, line, [{:nil, line}], [], [{:atom, line, :ok}]}
    clause_cons =
      {:clause, line, [{:cons, line, {:var, line, head_var}, {:var, line, rest_var}}], [], [body_case]}
    fun_expr = {:named_fun, line, fun_name, [clause_nil, clause_cons]}
    match_fun = {:match, line, {:var, line, fun_var}, fun_expr}
    collection_form =
      if iterator_type?(collection_type) do
        expr_form(line, {:field, %{target: collection, name: "data"}}, env)
      else
        expr_form(line, collection, env)
      end

    call_fun = {:call, line, {:var, line, fun_var}, [collection_form]}
    {{:block, line, [match_fun, call_fun]}, env, counter}
  end

  @spec loop_fun_expr(BeamLang.AST.expr() | :always_true, [BeamLang.AST.stmt()], map(), non_neg_integer(), atom()) ::
          {tuple(), map(), non_neg_integer()}
  defp loop_fun_expr(cond, body_stmts, env, counter, _kind) do
    line = 1
    {fun_var, counter} = fresh_var("loop", counter)
    fun_name = fun_var
    {body_expr, _env_body, counter} = stmt_expr_tree(body_stmts, env, counter)
    continue_call = {:call, line, {:var, line, fun_name}, []}
    {body_case, counter} = break_case(body_expr, continue_call, counter)

    cond_form =
      case cond do
        :always_true -> {:atom, line, true}
        _ -> expr_form(line, cond, env)
      end
    clause_true = {:clause, line, [{:atom, line, true}], [], [body_case]}
    clause_false = {:clause, line, [{:atom, line, false}], [], [{:atom, line, :ok}]}
    cond_case = {:case, line, cond_form, [clause_true, clause_false]}
    fun_expr = {:named_fun, line, fun_name, [{:clause, line, [], [], [cond_case]}]}
    match_fun = {:match, line, {:var, line, fun_var}, fun_expr}
    call_fun = {:call, line, {:var, line, fun_var}, []}
    {{:block, line, [match_fun, call_fun]}, env, counter}
  end

  @spec break_case(tuple(), tuple(), non_neg_integer()) :: {tuple(), non_neg_integer()}
  defp break_case(body_expr, continue_expr, counter) do
    line = 1
    ret_var = internal_var("return_value")
    return_tuple = {:tuple, line, [{:atom, line, :__return__}, {:var, line, ret_var}]}
    clause_return = {:clause, line, [return_tuple], [], [return_tuple]}
    clause_break = {:clause, line, [{:atom, line, :__break__}], [], [{:atom, line, :ok}]}
    clause_continue = {:clause, line, [{:var, line, :_}], [], [continue_expr]}
    {{:case, line, body_expr, [clause_return, clause_break, clause_continue]}, counter}
  end

  @spec block_expr_form(BeamLang.AST.block(), map()) :: tuple()
  defp block_expr_form({:block, %{stmts: stmts}}, env) do
    line = 1
    {expr, _env, _counter} = stmt_expr_tree(stmts, env, 0)
    clause = {:clause, line, [], [], [expr]}
    {:call, line, {:fun, line, {:clauses, [clause]}}, []}
  end

  @spec else_branch_expr(BeamLang.AST.if_else_branch(), map()) :: tuple()
  defp else_branch_expr({:else_block, %{block: block}}, env) do
    block_expr_form(block, env)
  end

  defp else_branch_expr({:else_if, %{if: if_stmt}}, env) do
    case if_stmt do
      {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}} ->
        cond_form = expr_form(1, cond, env)
        then_expr = block_expr_form(then_block, env)
        else_expr =
          case else_branch do
            nil -> {:atom, 1, :ok}
            _ -> else_branch_expr(else_branch, env)
          end

        clause_true = {:clause, 1, [{:atom, 1, true}], [], [then_expr]}
        clause_false = {:clause, 1, [{:atom, 1, false}], [], [else_expr]}
        {:case, 1, cond_form, [clause_true, clause_false]}
    end
  end

  @spec sequence_expr(tuple(), tuple(), non_neg_integer()) :: {tuple(), non_neg_integer()}
  defp sequence_expr(expr1, expr2, counter) do
    return_case(expr1, expr2, counter)
  end

  defp return_marker(expr) do
    line = 1
    {:tuple, line, [{:atom, line, :__return__}, expr]}
  end

  defp return_case(expr, continue_expr, counter) do
    line = 1
    ret_var = internal_var("return_value")
    return_tuple = {:tuple, line, [{:atom, line, :__return__}, {:var, line, ret_var}]}
    clause_return = {:clause, line, [return_tuple], [], [return_tuple]}
    clause_continue = {:clause, line, [{:var, line, :_}], [], [continue_expr]}
    {{:case, line, expr, [clause_return, clause_continue]}, counter}
  end

  defp unwrap_return(expr, counter) do
    line = 1
    ret_var = internal_var("return_value")
    other_var = internal_var("return_other")
    return_tuple = {:tuple, line, [{:atom, line, :__return__}, {:var, line, ret_var}]}
    clause_return = {:clause, line, [return_tuple], [], [{:var, line, ret_var}]}
    clause_other = {:clause, line, [{:var, line, other_var}], [], [{:var, line, other_var}]}
    {{:case, line, expr, [clause_return, clause_other]}, counter}
  end

  defp internal_var(name) do
    id = System.unique_integer([:positive])
    {first, rest} = String.split_at(name, 1)
    String.to_atom(String.upcase(first) <> rest <> "_" <> Integer.to_string(id))
  end

  defp iterator_type?({:generic, {:named, "Iterator"}, _}), do: true
  defp iterator_type?({:named, "Iterator"}), do: true
  defp iterator_type?(_), do: false

  defp lambda_params_form(params, line) do
    Enum.reduce(params, {[], %{}}, fn %{name: name}, {forms, env} ->
      var = var_atom(name)
      {forms ++ [{:var, line, var}], Map.put(env, name, var)}
    end)
  end

  defp block_stmts({:block, %{stmts: stmts}}), do: stmts


  @spec params_form([BeamLang.AST.func_param()], non_neg_integer(), map(), non_neg_integer()) ::
          {[tuple()], map(), non_neg_integer()}
  defp params_form(params, line, env, counter) do
    Enum.reduce(params, {[], env, counter}, fn %{name: name}, {acc, env_acc, counter_acc} ->
      {var, counter_acc} = fresh_var(name, counter_acc)
      param = {:var, line, var}
      {acc ++ [param], Map.put(env_acc, name, var), counter_acc}
    end)
  end

  @spec external_call(binary(), [BeamLang.AST.expr()], non_neg_integer(), map()) ::
          {:ok, tuple()} | :error
  defp external_call(name, args, line, env) do
    if String.contains?(name, "::") do
      :error
    else
      externals = Map.get(env, :__externals__, %{})

      case Map.fetch(externals, name) do
        {:ok, %{language: language, module: mod, function: fun}} ->
          mod_atom = external_module_atom(language, mod)
          fun_atom = String.to_atom(fun)
          {:ok, {:call, line, {:remote, line, {:atom, line, mod_atom}, {:atom, line, fun_atom}},
           Enum.map(args, &expr_form(line, &1, env))}}

        :error ->
          :error
      end
    end
  end

  defp external_module_atom("erlang", mod), do: String.to_atom(mod)
  defp external_module_atom("elixir", mod), do: String.to_atom("Elixir." <> mod)
  defp external_module_atom(_other, mod), do: String.to_atom(mod)

  @spec op_atom(BeamLang.AST.binary_op()) :: atom()
  defp op_atom(:add), do: :+
  defp op_atom(:sub), do: :-
  defp op_atom(:mul), do: :*
  defp op_atom(:div), do: :/
  defp op_atom(:mod), do: :rem
  defp op_atom(:eq), do: :"=:="
  defp op_atom(:neq), do: :"=/="
  defp op_atom(:lt), do: :<
  defp op_atom(:gt), do: :>
  defp op_atom(:lte), do: :"=<"
  defp op_atom(:gte), do: :>=

  defp module_atom(nil), do: @module_name
  defp module_atom(name) when is_binary(name), do: String.to_atom(name)

  defp qualified_name(name) when is_binary(name) do
    case String.split(name, "::", parts: 2) do
      [mod, fun] -> {:ok, mod, fun}
      _ -> :error
    end
  end

  defp type_label({:generic, base, args}) do
    "#{type_label(base)}<#{Enum.map_join(args, ", ", &type_label/1)}>"
  end

  defp type_label({:named, name}) when is_binary(name), do: name
  defp type_label({:optional, inner}), do: "#{type_label(inner)}?"
  defp type_label({:result, ok_type, err_type}), do: "#{type_label(ok_type)}!#{type_label(err_type)}"
  defp type_label({:fn, params, return_type}) do
    "fn(#{Enum.map_join(params, ", ", &type_label/1)}) -> #{type_label(return_type)}"
  end
  defp type_label(type) when is_atom(type), do: Atom.to_string(type)

  @spec case_clause_form(non_neg_integer(), BeamLang.AST.match_case(), map()) :: tuple()
  defp case_clause_form(line, %{pattern: pattern, guard: guard, body: body}, env) do
    {pat_form, pat_env, _counter} = pattern_form(line, pattern, 0)
    case_env = Map.merge(env, pat_env)

    body_expr =
      case body do
        {:block_expr, %{block: {:block, %{stmts: stmts}}}} ->
          {expr, _env, _counter} = stmt_expr_tree(stmts, case_env, 0)
          expr

        _ ->
          expr_form(line, body, case_env)
      end

    guards = case_guard_forms(line, guard, case_env)
    {:clause, line, [pat_form], guards, [body_expr]}
  end

  @spec case_guard_forms(non_neg_integer(), BeamLang.AST.expr() | nil, map()) :: list()
  defp case_guard_forms(_line, nil, _env), do: []

  defp case_guard_forms(line, guard, env) do
    [[guard_form(line, guard, env)]]
  end

  @spec guard_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp guard_form(line, {:binary, %{op: op, left: left, right: right}}, env) do
    {:op, line, op_atom(op), guard_operand_form(line, left, env),
     guard_operand_form(line, right, env)}
  end

  defp guard_form(line, expr, env), do: guard_operand_form(line, expr, env)

  @spec guard_operand_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp guard_operand_form(line, {:integer, %{value: value}}, _env), do: {:integer, line, value}
  defp guard_operand_form(line, {:float, %{value: value}}, _env), do: {:float, line, value}

  defp guard_operand_form(line, {:string, %{value: value}}, _env),
    do: {:string, line, String.to_charlist(value)}

  defp guard_operand_form(line, {:char, %{value: value}}, _env),
    do: {:tuple, line, [{:atom, line, :char}, {:integer, line, value}]}

  defp guard_operand_form(line, {:bool, %{value: value}}, _env), do: {:atom, line, value}

  defp guard_operand_form(line, {:identifier, %{name: name}}, env),
    do: {:var, line, Map.get(env, name, var_atom(name))}

  @spec pattern_form(non_neg_integer(), BeamLang.AST.pattern(), non_neg_integer()) ::
          {tuple(), map(), non_neg_integer()}
  defp pattern_form(line, {:wildcard, %{}}, counter) do
    {{:var, line, :_}, %{}, counter}
  end

  defp pattern_form(line, {:pat_identifier, %{name: name}}, counter) do
    if name == "_" do
      {{:var, line, :_}, %{}, counter}
    else
      var = internal_var(name)
      {{:var, line, var}, %{name => var}, counter}
    end
  end

  defp pattern_form(line, {:integer, %{value: value}}, counter) do
    {{:integer, line, value}, %{}, counter}
  end

  defp pattern_form(line, {:float, %{value: value}}, counter) do
    {{:float, line, value}, %{}, counter}
  end

  defp pattern_form(line, {:string, %{value: value}}, counter) do
    entries = [
      {:map_field_exact, line, {:atom, line, :data}, {:string, line, String.to_charlist(value)}}
    ]

    {{:map, line, entries}, %{}, counter}
  end

  defp pattern_form(line, {:char, %{value: value}}, counter) do
    {{:tuple, line, [{:atom, line, :char}, {:integer, line, value}]}, %{}, counter}
  end

  defp pattern_form(line, {:bool, %{value: value}}, counter) do
    {{:atom, line, value}, %{}, counter}
  end

  defp pattern_form(line, {:struct_pattern, %{fields: fields}}, counter) do
    {entries, env, counter} =
      Enum.reduce(fields, {[], %{}, counter}, fn %{name: name, pattern: pattern},
                                                 {acc, env_acc, counter_acc} ->
        {pat_form, pat_env, counter_acc} = pattern_form(line, pattern, counter_acc)
        entry = {:map_field_exact, line, {:atom, line, String.to_atom(name)}, pat_form}
        {acc ++ [entry], Map.merge(env_acc, pat_env), counter_acc}
      end)

    {{:map, line, entries}, env, counter}
  end

  defp pattern_form(line, {:opt_some_pat, %{name: name}}, counter) do
    {pat, env, counter} =
      if name == "_" do
        {{:var, line, :_}, %{}, counter}
      else
        var = internal_var(name)
        {{:var, line, var}, %{name => var}, counter}
      end

    entries = [
      {:map_field_exact, line, {:atom, line, :tag}, {:integer, line, 1}},
      {:map_field_exact, line, {:atom, line, :value}, pat}
    ]

    {{:map, line, entries}, env, counter}
  end

  defp pattern_form(line, {:opt_none_pat, %{}}, counter) do
    entries = [
      {:map_field_exact, line, {:atom, line, :tag}, {:integer, line, 0}}
    ]

    {{:map, line, entries}, %{}, counter}
  end

  defp pattern_form(line, {:res_ok_pat, %{name: name}}, counter) do
    {pat, env, counter} =
      if name == "_" do
        {{:var, line, :_}, %{}, counter}
      else
        var = internal_var(name)
        {{:var, line, var}, %{name => var}, counter}
      end

    entries = [
      {:map_field_exact, line, {:atom, line, :tag}, {:integer, line, 1}},
      {:map_field_exact, line, {:atom, line, :value}, pat}
    ]

    {{:map, line, entries}, env, counter}
  end

  defp pattern_form(line, {:res_err_pat, %{name: name}}, counter) do
    {pat, env, counter} =
      if name == "_" do
        {{:var, line, :_}, %{}, counter}
      else
        var = internal_var(name)
        {{:var, line, var}, %{name => var}, counter}
      end

    entries = [
      {:map_field_exact, line, {:atom, line, :tag}, {:integer, line, 0}},
      {:map_field_exact, line, {:atom, line, :value}, pat}
    ]

    {{:map, line, entries}, env, counter}
  end

  @spec var_atom(binary()) :: atom()
  defp var_atom(name) do
    {first, rest} = String.split_at(name, 1)
    String.to_atom(String.upcase(first) <> rest)
  end

  @spec fresh_var(binary(), non_neg_integer()) :: {atom(), non_neg_integer()}
  defp fresh_var(name, counter) do
    {base_first, base_rest} = String.split_at(name, 1)
    var = String.to_atom(String.upcase(base_first) <> base_rest <> "_#{counter}")
    {var, counter + 1}
  end
end
