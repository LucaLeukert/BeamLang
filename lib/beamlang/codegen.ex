defmodule BeamLang.Codegen do
  @moduledoc """
  Generates Erlang abstract forms from BeamLang AST.
  """

  @module_name :beamlang_program

  @spec module_name() :: atom()
  def module_name, do: @module_name

  @spec to_erlang_forms(BeamLang.AST.t()) :: list()
  def to_erlang_forms({:program, %{functions: functions}}) when is_list(functions) do
    line = 1

    function_forms = Enum.map(functions, &function_form/1)

    [
      {:attribute, line, :module, @module_name},
      {:attribute, line, :export, exports(functions)}
      | function_forms
    ]
  end

  @spec exports([BeamLang.AST.func()]) :: list()
  defp exports(functions) do
    Enum.map(functions, fn {:function, %{name: name, params: params}} ->
      {String.to_atom(name), length(params)}
    end)
  end

  @spec function_form(BeamLang.AST.func()) :: tuple()
  defp function_form({:function, %{name: name, params: params, body: {:block, %{stmts: stmts}}}}) do
    line = 1
    {params_form, env, counter} = params_form(params, line, %{}, 0)
    {expr, _env, _counter} = stmt_expr_tree(stmts, env, counter)

    {:function, line, String.to_atom(name), length(params),
     [return_clause(line, params_form, [expr])]}
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

      {:for, %{name: name, collection: collection, body: {:block, %{stmts: body_stmts}}}} ->
        {expr, env, counter} = for_expr(name, collection, body_stmts, env, counter)
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
        {{:block, 1, [loop_expr, rest_expr]}, env, counter}

      {:loop, %{body: {:block, %{stmts: body_stmts}}}} ->
        {loop_expr, env, counter} = loop_expr(body_stmts, env, counter)
        {rest_expr, env, counter} = stmt_expr_tree(rest, env, counter)
        {{:block, 1, [loop_expr, rest_expr]}, env, counter}

      {:for, %{name: name, collection: collection, body: {:block, %{stmts: body_stmts}}}} ->
        {for_expr, env, counter} = for_expr(name, collection, body_stmts, env, counter)
        {rest_expr, env, counter} = stmt_expr_tree(rest, env, counter)
        {{:block, 1, [for_expr, rest_expr]}, env, counter}

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
        {{:block, 1, [form, rest_expr]}, env, counter}
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
    {{:atom, 1, :ok}, env, counter}
  end

  defp stmt_form({:return, %{expr: expr}}, env, counter) do
    {expr_form(1, expr, env), env, counter}
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
    {:string, line, String.to_charlist(value)}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:struct, %{fields: fields}}, env) do
    entries =
      Enum.map(fields, fn %{name: name, expr: expr} ->
        {:map_field_assoc, line, {:atom, line, String.to_atom(name)}, expr_form(line, expr, env)}
      end)

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
    case stdlib_call(name, args, line, env) do
      {:ok, form} ->
        form

      :error ->
        {:call, line, {:atom, line, String.to_atom(name)},
         Enum.map(args, &expr_form(line, &1, env))}
    end
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:identifier, %{name: name}}, env) do
    {:var, line, Map.get(env, name, var_atom(name))}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:binary, %{op: op, left: left, right: right}}, env) do
    {:op, line, op_atom(op), expr_form(line, left, env), expr_form(line, right, env)}
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

    then_seq = sequence_expr(then_expr, rest_expr)
    else_seq = if else_branch == nil, do: rest_expr, else: sequence_expr(else_expr, rest_expr)

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

  @spec for_expr(binary(), BeamLang.AST.expr(), [BeamLang.AST.stmt()], map(), non_neg_integer()) ::
          {tuple(), map(), non_neg_integer()}
  defp for_expr(name, collection, body_stmts, env, counter) do
    line = 1
    {fun_var, counter} = fresh_var("for_loop", counter)
    fun_name = fun_var
    {item_var, counter} = fresh_var(name, counter)
    {rest_var, counter} = fresh_var("rest", counter)

    body_env = Map.put(env, name, item_var)
    {body_expr, _env_body, counter} = stmt_expr_tree(body_stmts, body_env, counter)
    continue_call = {:call, line, {:var, line, fun_name}, [{:var, line, rest_var}]}
    body_case = break_case(body_expr, continue_call)

    clause_nil = {:clause, line, [{:nil, line}], [], [{:atom, line, :ok}]}
    clause_cons = {:clause, line, [{:cons, line, {:var, line, item_var}, {:var, line, rest_var}}], [], [body_case]}
    fun_expr = {:named_fun, line, fun_name, [clause_nil, clause_cons]}
    match_fun = {:match, line, {:var, line, fun_var}, fun_expr}
    call_fun = {:call, line, {:var, line, fun_var}, [expr_form(line, collection, env)]}
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
    body_case = break_case(body_expr, continue_call)

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

  @spec break_case(tuple(), tuple()) :: tuple()
  defp break_case(body_expr, continue_expr) do
    line = 1
    clause_break = {:clause, line, [{:atom, line, :__break__}], [], [{:atom, line, :ok}]}
    clause_continue = {:clause, line, [{:var, line, :_}], [], [continue_expr]}
    {:case, line, body_expr, [clause_break, clause_continue]}
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

  @spec sequence_expr(tuple(), tuple()) :: tuple()
  defp sequence_expr(expr1, expr2) do
    {:block, 1, [expr1, expr2]}
  end


  @spec params_form([BeamLang.AST.func_param()], non_neg_integer(), map(), non_neg_integer()) ::
          {[tuple()], map(), non_neg_integer()}
  defp params_form(params, line, env, counter) do
    Enum.reduce(params, {[], env, counter}, fn %{name: name}, {acc, env_acc, counter_acc} ->
      {var, counter_acc} = fresh_var(name, counter_acc)
      param = {:var, line, var}
      {acc ++ [param], Map.put(env_acc, name, var), counter_acc}
    end)
  end

  @spec stdlib_call(binary(), [BeamLang.AST.expr()], non_neg_integer(), map()) ::
          {:ok, tuple()} | :error
  defp stdlib_call("println", [arg], line, env) do
    {:ok,
     {:call, line, {:remote, line, {:atom, line, :io}, {:atom, line, :format}},
      [
        {:string, line, ~c"~s~n"},
        list_form(line, [expr_form(line, arg, env)])
      ]}}
  end

  defp stdlib_call("print", [arg], line, env) do
    {:ok,
     {:call, line, {:remote, line, {:atom, line, :io}, {:atom, line, :format}},
      [
        {:string, line, ~c"~s"},
        list_form(line, [expr_form(line, arg, env)])
      ]}}
  end

  defp stdlib_call(_name, _args, _line, _env), do: :error

  @spec list_form(non_neg_integer(), [tuple()]) :: tuple()
  defp list_form(line, []), do: {:nil, line}

  defp list_form(line, [head | tail]) do
    {:cons, line, head, list_form(line, tail)}
  end

  @spec op_atom(BeamLang.AST.binary_op()) :: atom()
  defp op_atom(:eq), do: :"=:="
  defp op_atom(:neq), do: :"=/="
  defp op_atom(:lt), do: :<
  defp op_atom(:gt), do: :>
  defp op_atom(:lte), do: :"=<"
  defp op_atom(:gte), do: :>=

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

  defp guard_operand_form(line, {:bool, %{value: value}}, _env), do: {:atom, line, value}

  defp guard_operand_form(line, {:identifier, %{name: name}}, env),
    do: {:var, line, Map.get(env, name, var_atom(name))}

  @spec pattern_form(non_neg_integer(), BeamLang.AST.pattern(), non_neg_integer()) ::
          {tuple(), map(), non_neg_integer()}
  defp pattern_form(line, {:wildcard, %{}}, counter) do
    {{:var, line, :_}, %{}, counter}
  end

  defp pattern_form(line, {:pat_identifier, %{name: name}}, counter) do
    {var, counter} = fresh_var(name, counter)
    {{:var, line, var}, %{name => var}, counter}
  end

  defp pattern_form(line, {:integer, %{value: value}}, counter) do
    {{:integer, line, value}, %{}, counter}
  end

  defp pattern_form(line, {:float, %{value: value}}, counter) do
    {{:float, line, value}, %{}, counter}
  end

  defp pattern_form(line, {:string, %{value: value}}, counter) do
    {{:string, line, String.to_charlist(value)}, %{}, counter}
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
