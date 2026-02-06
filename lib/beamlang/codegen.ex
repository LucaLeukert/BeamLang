defmodule BeamLang.Codegen do
  @moduledoc """
  Generates Erlang abstract forms from BeamLang AST.
  """

  @module_name :beamlang_program

  @spec to_erlang_forms(BeamLang.AST.t()) :: list()
  def to_erlang_forms({:program, %{module: module, functions: functions}})
      when is_list(functions) do
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

  # Internal stdlib constructors that need to be exported for Runtime use
  @runtime_required_internals ~w(
    string_new list_from_data iterator_from_list
    result_ok result_err optional_some optional_none
    file_entry_new http_response_new
  )

  @spec exports([BeamLang.AST.func()]) :: list()
  defp exports(functions) do
    functions
    |> Enum.filter(&function_has_body?/1)
    |> Enum.filter(fn {:function, %{name: name, exported: exported, internal: internal}} ->
      name == "main" or (exported and not internal) or name in @runtime_required_internals
    end)
    |> Enum.map(fn {:function, %{name: name, params: params}} ->
      {String.to_atom(name), length(params)}
    end)
  end

  @spec function_form(BeamLang.AST.func(), map(), map()) :: tuple()
  defp function_form(
         {:function, %{name: name, params: params, body: {:block, %{stmts: stmts}}}},
         externals,
         functions_map
       ) do
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
  defp stmt_expr_tree(stmts, env, counter) do
    stmt_expr_tree_with_final(stmts, env, counter, {:atom, 1, :ok})
  end

  @spec stmt_expr_tree_with_final([BeamLang.AST.stmt()], map(), non_neg_integer(), tuple()) ::
          {tuple(), map(), non_neg_integer()}
  defp stmt_expr_tree_with_final([], env, counter, final_expr) do
    {final_expr, env, counter}
  end

  defp stmt_expr_tree_with_final([stmt], env, counter, final_expr) do
    if final_expr == {:atom, 1, :ok} do
      case stmt do
        {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}} ->
          {expr, env, counter} =
            if_stmt_expr(cond, then_block, else_branch, env, counter, final_expr)

          {expr, env, counter}

        {:while, %{cond: cond, body: {:block, %{stmts: body_stmts}}}} ->
          {expr, env, counter} = while_expr(cond, body_stmts, env, counter)
          {expr, env, counter}

        {:loop, %{body: {:block, %{stmts: body_stmts}}}} ->
          {expr, env, counter} = loop_expr(body_stmts, env, counter)
          {expr, env, counter}

        {:for, %{name: name, collection: collection, body: {:block, %{stmts: body_stmts}}} = info} ->
          collection_type = Map.get(info, :collection_type, :any)

          {expr, env, counter} =
            for_expr(name, collection, body_stmts, env, counter, collection_type)

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
    else
      case stmt do
        {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}} ->
          {expr, env, counter} =
            if_stmt_expr(cond, then_block, else_branch, env, counter, final_expr)

          {expr, env, counter}

        {:while, %{cond: cond, body: {:block, %{stmts: body_stmts}}}} ->
          {expr, env, counter} = while_expr(cond, body_stmts, env, counter)
          {expr, counter} = sequence_expr(expr, final_expr, counter)
          {expr, env, counter}

        {:loop, %{body: {:block, %{stmts: body_stmts}}}} ->
          {expr, env, counter} = loop_expr(body_stmts, env, counter)
          {expr, counter} = sequence_expr(expr, final_expr, counter)
          {expr, env, counter}

        {:for, %{name: name, collection: collection, body: {:block, %{stmts: body_stmts}}} = info} ->
          collection_type = Map.get(info, :collection_type, :any)

          {expr, env, counter} =
            for_expr(name, collection, body_stmts, env, counter, collection_type)

          {expr, counter} = sequence_expr(expr, final_expr, counter)
          {expr, env, counter}

        {:guard, %{cond: cond, else_block: {:block, %{stmts: else_stmts}}}} ->
          cond_form = expr_form(1, cond, env)
          {else_expr, _else_env, counter} = stmt_expr_tree(else_stmts, env, counter)

          clause_true = {:clause, 1, [{:atom, 1, true}], [], [final_expr]}
          clause_false = {:clause, 1, [{:atom, 1, false}], [], [else_expr]}

          {{:case, 1, cond_form, [clause_true, clause_false]}, env, counter}

        _ ->
          {form, env, counter} = stmt_form(stmt, env, counter)
          {expr, counter} = sequence_expr(form, final_expr, counter)
          {expr, env, counter}
      end
    end
  end

  defp stmt_expr_tree_with_final([stmt | rest], env, counter, final_expr) do
    case stmt do
      {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}} ->
        # Find all variables assigned in then/else blocks
        then_stmts = case then_block do
          {:block, %{stmts: stmts}} -> stmts
          _ -> []
        end
        {else_stmts, else_if_vars} =
          case else_branch do
            {:else_block, %{block: {:block, %{stmts: stmts}}}} -> {stmts, []}
            {:else_if, %{if: if_stmt}} -> {[], find_assigned_in_stmt(if_stmt)}
            nil -> {[], []}
          end

        assigned_vars =
          (find_assigned_vars(then_stmts) ++ find_assigned_vars(else_stmts) ++ else_if_vars)
          |> Enum.uniq()
          |> Enum.filter(&Map.has_key?(env, &1))

        if Enum.empty?(assigned_vars) do
          # No variable changes - use simple approach
          {rest_expr, rest_env, counter} = stmt_expr_tree_with_final(rest, env, counter, final_expr)
          {expr, _env, counter} =
            if_stmt_expr(cond, then_block, else_branch, env, counter, rest_expr)
          {expr, rest_env, counter}
        else
          # Variables are modified - need to propagate them
          {if_expr, new_env, counter} = if_stmt_expr_with_propagation(
            cond, then_block, else_branch, env, counter, assigned_vars
          )
          # Compile rest with the new env containing updated variable names
          {rest_expr, rest_env, counter} = stmt_expr_tree_with_final(rest, new_env, counter, final_expr)
          {expr, counter} = sequence_expr(if_expr, rest_expr, counter)
          {expr, rest_env, counter}
        end

      {:while, %{cond: cond, body: {:block, %{stmts: body_stmts}}}} ->
        {loop_expr, env, counter} = while_expr(cond, body_stmts, env, counter)
        {rest_expr, env, counter} = stmt_expr_tree_with_final(rest, env, counter, final_expr)
        {expr, counter} = sequence_expr(loop_expr, rest_expr, counter)
        {expr, env, counter}

      {:loop, %{body: {:block, %{stmts: body_stmts}}}} ->
        {loop_expr, env, counter} = loop_expr(body_stmts, env, counter)
        {rest_expr, env, counter} = stmt_expr_tree_with_final(rest, env, counter, final_expr)
        {expr, counter} = sequence_expr(loop_expr, rest_expr, counter)
        {expr, env, counter}

      {:for, %{name: name, collection: collection, body: {:block, %{stmts: body_stmts}}} = info} ->
        collection_type = Map.get(info, :collection_type, :any)

        {for_expr, env, counter} =
          for_expr(name, collection, body_stmts, env, counter, collection_type)

        {rest_expr, env, counter} = stmt_expr_tree_with_final(rest, env, counter, final_expr)
        {expr, counter} = sequence_expr(for_expr, rest_expr, counter)
        {expr, env, counter}

      {:guard, %{cond: cond, else_block: {:block, %{stmts: else_stmts}}}} ->
        cond_form = expr_form(1, cond, env)
        {else_expr, _else_env, counter} = stmt_expr_tree(else_stmts, env, counter)
        {rest_expr, env, counter} = stmt_expr_tree_with_final(rest, env, counter, final_expr)

        clause_true = {:clause, 1, [{:atom, 1, true}], [], [rest_expr]}
        clause_false = {:clause, 1, [{:atom, 1, false}], [], [else_expr]}

        {{:case, 1, cond_form, [clause_true, clause_false]}, env, counter}

      _ ->
        {form, env, counter} = stmt_form(stmt, env, counter)
        {rest_expr, env, counter} = stmt_expr_tree_with_final(rest, env, counter, final_expr)
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

  defp stmt_form({:let_destruct, %{pattern: pattern, expr: expr}}, env, counter) do
    case pattern do
      {:struct_destruct, %{fields: fields}} ->
        # Generate: {pattern_form, expr_form}
        expr_form_val = expr_form(1, expr, env)
        {bindings, env, counter} = destruct_struct_bindings(fields, expr_form_val, env, counter)
        form = destruct_struct_form(1, bindings, expr_form_val)
        {form, env, counter}

      {:tuple_destruct, %{elements: elements}} ->
        expr_form_val = expr_form(1, expr, env)
        {bindings, env, counter} = destruct_tuple_bindings(elements, env, counter)
        form = destruct_tuple_form(1, bindings, expr_form_val)
        {form, env, counter}
    end
  end

  defp stmt_form({:assign, %{target: target, expr: expr}}, env, counter) do
    case assignment_form(1, target, expr, env, counter) do
      {:ok, form, new_env, new_counter} -> {form, new_env, new_counter}
      {:error, _} -> {{:atom, 1, :ok}, env, counter}
    end
  end

  defp stmt_form({:compound_assign, %{target: target, op: op, expr: expr}}, env, counter) do
    # Transform x += y into x = x + y
    binary_expr = {:binary, %{op: op, left: target, right: expr, span: BeamLang.Span.new("<generated>", 0, 0)}}
    case assignment_form(1, target, binary_expr, env, counter) do
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
    case Map.get(env, :__loop_vars__) do
      nil ->
        {{:atom, 1, :__break__}, env, counter}

      vars ->
        break_values = build_vars_tuple(1, vars, env)
        {{:tuple, 1, [{:atom, 1, :__break__}, break_values]}, env, counter}
    end
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
  defp expr_form(line, {:interpolated_string, %{string: template, expressions: expressions}}, env) do
    # Split the template string by {} placeholders
    parts = String.split(template, "{}")

    # Convert expressions to their forms
    expr_forms = Enum.map(expressions, fn expr -> expr_form(line, expr, env) end)

    # Build the interpolated string by concatenating parts and expressions
    build_interpolated_string(line, parts, expr_forms)
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:char, %{value: value}}, _env) do
    {:tuple, line, [{:atom, line, :char}, {:integer, line, value}]}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:struct, %{fields: fields} = struct_data}, env) do
    entries =
      Enum.map(fields, fn %{name: name, expr: expr} ->
        {:map_field_assoc, line, {:atom, line, String.to_atom(name)}, expr_form(line, expr, env)}
      end)

    # Add operator bindings as __op_* fields with function references
    operators = Map.get(struct_data, :operators, [])
    operator_entries =
      Enum.map(operators, fn %{op: op, func: func_name} ->
        field_name = String.to_atom("__op_#{op}")
        # Get the function arity from the environment
        arity = Map.get(env[:__functions__], func_name, 2)
        func_atom = String.to_atom(func_name)
        # Create a fun reference: fun func_name/arity
        func_ref = {:fun, line, {:function, func_atom, arity}}
        {:map_field_assoc, line, {:atom, line, field_name}, func_ref}
      end)

    entries = entries ++ operator_entries

    type = Map.get(struct_data, :type)
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
  defp expr_form(
         line,
         {:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch}},
         env
       ) do
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
  defp expr_form(
         line,
         {:call,
          %{
            name: "parse_args",
            args: [args_expr],
            type_info: %{fields: fields, field_types: field_types, field_annotations: field_annotations, type: type}
          }},
         env
       ) do
    # Build the fields list as Erlang list of binaries
    fields_list = build_list_form(line, Enum.map(fields, fn f -> {:string, line, String.to_charlist(f)} end))

    # Build the field_types list as Erlang atoms/tuples
    types_list = build_list_form(line, Enum.map(field_types, fn t -> type_to_erlang_term(line, t) end))

    # Build the annotations list - each field's annotations as list of {name, args} tuples
    annotations_list = build_list_form(line, Enum.map(field_annotations, fn anns ->
      ann_forms = Enum.map(anns, fn %{name: name, args: args} ->
        args_form = build_list_form(line, Enum.map(args, &annotation_arg_form(line, &1)))
        {:tuple, line, [{:string, line, String.to_charlist(name)}, args_form]}
      end)
      build_list_form(line, ann_forms)
    end))

    # Get the type label
    type_label = {:string, line, String.to_charlist(type_label(type))}

    # Call BeamLang.Runtime.parse_args(fields, field_types, annotations, type_label, args)
    {:call, line,
     {:remote, line, {:atom, line, :"Elixir.BeamLang.Runtime"}, {:atom, line, :parse_args}},
     [fields_list, types_list, annotations_list, type_label, expr_form(line, args_expr, env)]}
  end

  # usage<T>(program) - generate help text from type annotations
  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(
         line,
         {:call,
          %{
            name: "usage",
            args: [program_expr],
            type_info: %{fields: fields, field_types: field_types, field_annotations: field_annotations, type: type}
          }},
         env
       ) do
    # Build the fields list as Erlang list of binaries
    fields_list = build_list_form(line, Enum.map(fields, fn f -> {:string, line, String.to_charlist(f)} end))

    # Build the field_types list as Erlang atoms/tuples
    types_list = build_list_form(line, Enum.map(field_types, fn t -> type_to_erlang_term(line, t) end))

    # Build the annotations list
    annotations_list = build_list_form(line, Enum.map(field_annotations, fn anns ->
      ann_forms = Enum.map(anns, fn %{name: name, args: args} ->
        args_form = build_list_form(line, Enum.map(args, &annotation_arg_form(line, &1)))
        {:tuple, line, [{:string, line, String.to_charlist(name)}, args_form]}
      end)
      build_list_form(line, ann_forms)
    end))

    # Get the type label
    type_label = {:string, line, String.to_charlist(type_label(type))}

    # Call BeamLang.Runtime.args_usage(fields, field_types, annotations, type_label, program)
    {:call, line,
     {:remote, line, {:atom, line, :"Elixir.BeamLang.Runtime"}, {:atom, line, :args_usage}},
     [fields_list, types_list, annotations_list, type_label, expr_form(line, program_expr, env)]}
  end

  # usage<T>(program) - fallback without annotations
  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(
         line,
         {:call,
          %{
            name: "usage",
            args: [program_expr],
            type_info: %{fields: fields, field_types: field_types, type: type}
          }},
         env
       ) do
    # Build the fields list as Erlang list of binaries
    fields_list = build_list_form(line, Enum.map(fields, fn f -> {:string, line, String.to_charlist(f)} end))

    # Build the field_types list as Erlang atoms/tuples
    types_list = build_list_form(line, Enum.map(field_types, fn t -> type_to_erlang_term(line, t) end))

    # Empty annotations list
    annotations_list = build_list_form(line, Enum.map(fields, fn _ -> {:nil, line} end))

    # Get the type label
    type_label = {:string, line, String.to_charlist(type_label(type))}

    # Call BeamLang.Runtime.args_usage(fields, field_types, annotations, type_label, program)
    {:call, line,
     {:remote, line, {:atom, line, :"Elixir.BeamLang.Runtime"}, {:atom, line, :args_usage}},
     [fields_list, types_list, annotations_list, type_label, expr_form(line, program_expr, env)]}
  end

  # Fallback for parse_args without annotations (backwards compatibility)
  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(
         line,
         {:call,
          %{
            name: "parse_args",
            args: [args_expr],
            type_info: %{fields: fields, field_types: field_types, type: type}
          }},
         env
       ) do
    # Build the fields list as Erlang list of binaries
    fields_list = build_list_form(line, Enum.map(fields, fn f -> {:string, line, String.to_charlist(f)} end))

    # Build the field_types list as Erlang atoms/tuples
    types_list = build_list_form(line, Enum.map(field_types, fn t -> type_to_erlang_term(line, t) end))

    # Empty annotations list for backwards compatibility
    annotations_list = build_list_form(line, Enum.map(fields, fn _ -> {:nil, line} end))

    # Get the type label
    type_label = {:string, line, String.to_charlist(type_label(type))}

    # Call BeamLang.Runtime.parse_args(fields, field_types, annotations, type_label, args)
    {:call, line,
     {:remote, line, {:atom, line, :"Elixir.BeamLang.Runtime"}, {:atom, line, :parse_args}},
     [fields_list, types_list, annotations_list, type_label, expr_form(line, args_expr, env)]}
  end

  @spec expr_form(non_neg_integer(), BeamLang.AST.expr(), map()) :: tuple()
  defp expr_form(line, {:call, %{name: name, args: args}}, env) do
    case external_call(name, args, line, env) do
      {:ok, form} ->
        form

      :error ->
        if Map.has_key?(env, name) do
          {:call, line, {:var, line, Map.get(env, name)},
           Enum.map(args, &expr_form(line, &1, env))}
        else
          case qualified_name(name) do
            {:ok, mod, fun} ->
              {:call, line,
               {:remote, line, {:atom, line, String.to_atom(mod)},
                {:atom, line, String.to_atom(fun)}}, Enum.map(args, &expr_form(line, &1, env))}

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
    all_args = [expr_form(line, target, env) | Enum.map(args, &expr_form(line, &1, env))]
    {:call, line, fun_expr, all_args}
  end

  defp expr_form(line, {:binary, %{op: op, left: left, right: right} = info}, env) do
    # Check if this is an overloaded operator
    case Map.get(info, :operator_info) do
      %{overloaded: true, func_name: func_name} ->
        # Check if it's an __op_* field (instance method) or a regular function
        if String.starts_with?(func_name, "__op_") do
          # Call the operator method from the left operand's field
          fun_expr = expr_form(line, {:field, %{target: left, name: func_name}}, env)
          all_args = [expr_form(line, left, env), expr_form(line, right, env)]
          {:call, line, fun_expr, all_args}
        else
          # Call the operator function directly (naming convention)
          all_args = [expr_form(line, left, env), expr_form(line, right, env)]
          {:call, line, {:atom, line, String.to_atom(func_name)}, all_args}
        end

      _ ->
        # Standard operator handling
        case op do
          :add ->
            left_type = get_expr_type(left)
            right_type = get_expr_type(right)

            if is_string_type?(left_type) or is_string_type?(right_type) do
              # String concatenation: convert both to strings and concat
              left_form = ensure_string_form(line, left, left_type, env)
              right_form = ensure_string_form(line, right, right_type, env)

              fun_expr =
                {:call, line, {:remote, line, {:atom, line, :maps}, {:atom, line, :get}},
                 [{:atom, line, :concat}, left_form]}

              {:call, line, fun_expr, [left_form, right_form]}
            else
              # Numeric addition
              {:op, line, :+, expr_form(line, left, env), expr_form(line, right, env)}
            end

          _ ->
            {:op, line, op_atom(op), expr_form(line, left, env), expr_form(line, right, env)}
        end
    end
  end

  defp expr_form(line, {:opt_some, %{expr: expr}}, env) do
    value = expr_form(line, expr, env)
    {:call, line, {:atom, line, :optional_some}, [value]}
  end

  defp expr_form(line, {:opt_none, %{}}, _env) do
    {:call, line, {:atom, line, :optional_none}, []}
  end

  defp expr_form(line, {:res_ok, %{expr: expr}}, env) do
    value = expr_form(line, expr, env)
    {:call, line, {:atom, line, :result_ok}, [value]}
  end

  defp expr_form(line, {:res_err, %{expr: expr}}, env) do
    value = expr_form(line, expr, env)
    {:call, line, {:atom, line, :result_err}, [value]}
  end

  defp expr_form(line, {:try_expr, %{expr: inner_expr, kind: kind}}, env) do
    # Generate: case inner_expr of
    #   #{tag: 1, value: V} -> V   (ok/some)
    #   #{tag: 0, value: E} = Err -> throw({:__try_propagate__, Err})  (err/none)
    inner_form = expr_form(line, inner_expr, env)

    ok_pattern = {:map, line, [
      {:map_field_exact, line, {:atom, line, :tag}, {:integer, line, 1}},
      {:map_field_exact, line, {:atom, line, :value}, {:var, line, :__TryValue__}}
    ]}
    ok_clause = {:clause, line, [ok_pattern], [], [{:var, line, :__TryValue__}]}

    case kind do
      :result ->
        err_pattern = {:map, line, [
          {:map_field_exact, line, {:atom, line, :tag}, {:integer, line, 0}},
          {:map_field_exact, line, {:atom, line, :value}, {:var, line, :__TryErr__}}
        ]}
        # Return the error wrapped as result_err
        err_result = {:call, line, {:atom, line, :result_err}, [{:var, line, :__TryErr__}]}
        throw_expr = {:call, line, {:remote, line, {:atom, line, :erlang}, {:atom, line, :throw}},
          [{:tuple, line, [{:atom, line, :__try_propagate__}, err_result]}]}
        err_clause = {:clause, line, [err_pattern], [], [throw_expr]}
        {:case, line, inner_form, [ok_clause, err_clause]}

      :optional ->
        none_pattern = {:map, line, [
          {:map_field_exact, line, {:atom, line, :tag}, {:integer, line, 0}}
        ]}
        none_result = {:call, line, {:atom, line, :optional_none}, []}
        throw_expr = {:call, line, {:remote, line, {:atom, line, :erlang}, {:atom, line, :throw}},
          [{:tuple, line, [{:atom, line, :__try_propagate__}, none_result]}]}
        none_clause = {:clause, line, [none_pattern], [], [throw_expr]}
        {:case, line, inner_form, [ok_clause, none_clause]}
    end
  end

  defp expr_form(line, {:tuple, %{elements: elements}}, env) do
    elem_forms = Enum.map(elements, fn elem -> expr_form(line, elem, env) end)
    {:tuple, line, elem_forms}
  end

  defp expr_form(line, {:enum_variant, %{enum_name: enum_name, variant: variant, fields: fields}}, env) do
    # Generate: %{__enum__: "EnumName", __variant__: "Variant", field1: value1, ...}
    base_fields = [
      {:map_field_assoc, line, {:atom, line, :__enum__}, {:bin, line, [{:bin_element, line, {:string, line, String.to_charlist(enum_name)}, :default, :default}]}},
      {:map_field_assoc, line, {:atom, line, :__variant__}, {:bin, line, [{:bin_element, line, {:string, line, String.to_charlist(variant)}, :default, :default}]}}
    ]

    field_forms = Enum.map(fields, fn {name, expr} ->
      {:map_field_assoc, line, {:atom, line, String.to_atom(name)}, expr_form(line, expr, env)}
    end)

    {:map, line, base_fields ++ field_forms}
  end

  defp expr_form(line, {:range, %{start: start_expr, end: end_expr}}, env) do
    # Compile 1..10 to range(1, 10)
    start_form = expr_form(line, start_expr, env)
    end_form = expr_form(line, end_expr, env)
    {:call, line, {:atom, line, :range}, [start_form, end_form]}
  end

  defp expr_form(line, {:list_literal, %{elements: elements}}, env) do
    elem_forms = Enum.map(elements, fn elem -> expr_form(line, elem, env) end)
    list_form = build_list_form(line, elem_forms)

    {:call, line, {:atom, line, :list_of}, [list_form]}
  end

  defp build_list_form(line, []), do: {:nil, line}

  defp build_list_form(line, [head | tail]) do
    {:cons, line, head, build_list_form(line, tail)}
  end

  # Convert annotation argument to Erlang form
  defp annotation_arg_form(line, arg) when is_binary(arg), do: {:string, line, String.to_charlist(arg)}
  defp annotation_arg_form(line, arg) when is_integer(arg), do: {:integer, line, arg}
  defp annotation_arg_form(line, arg) when is_float(arg), do: {:float, line, arg}
  defp annotation_arg_form(line, arg) when is_boolean(arg), do: {:atom, line, arg}
  defp annotation_arg_form(line, arg) when is_atom(arg), do: {:atom, line, arg}
  defp annotation_arg_form(line, _arg), do: {:nil, line}

  # Convert BeamLang type to Erlang term for runtime
  defp type_to_erlang_term(line, :String), do: {:atom, line, :String}
  defp type_to_erlang_term(line, :number), do: {:atom, line, :number}
  defp type_to_erlang_term(line, :bool), do: {:atom, line, :bool}
  defp type_to_erlang_term(line, :char), do: {:atom, line, :char}
  defp type_to_erlang_term(line, {:named, name}) do
    {:tuple, line, [{:atom, line, :named}, {:string, line, String.to_charlist(name)}]}
  end
  defp type_to_erlang_term(line, _), do: {:atom, line, :unknown}

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

  @spec if_stmt_expr(
          BeamLang.AST.expr(),
          BeamLang.AST.block(),
          BeamLang.AST.if_else_branch() | nil,
          map(),
          non_neg_integer(),
          tuple()
        ) ::
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

  # If statement with variable propagation - returns updated variables as tuple
  @spec if_stmt_expr_with_propagation(
          BeamLang.AST.expr(),
          BeamLang.AST.block(),
          BeamLang.AST.if_else_branch() | nil,
          map(),
          non_neg_integer(),
          [binary()]
        ) ::
          {tuple(), map(), non_neg_integer()}
  defp if_stmt_expr_with_propagation(cond, then_block, else_branch, env, counter, assigned_vars) do
    line = 1

    # Generate fresh variable names for the results
    {result_vars, counter} = Enum.reduce(assigned_vars, {[], counter}, fn name, {acc, cnt} ->
      {var, cnt} = fresh_var(name, cnt)
      {[{name, var} | acc], cnt}
    end)
    result_vars = Enum.reverse(result_vars)

    {flow_expr, counter} = if_flow_expr(cond, then_block, else_branch, env, counter, assigned_vars)

    # Build the pattern for the result tuple
    pattern = build_pattern_tuple(line, result_vars)

    {flow_tag, counter} = fresh_var("flow_tag", counter)
    {flow_payload, counter} = fresh_var("flow_payload", counter)

    flow_pattern =
      {:tuple, line,
       [
         {:atom, line, :__flow__},
         {:var, line, flow_tag},
         {:var, line, flow_payload},
         pattern
       ]}

    match_expr = {:match, line, flow_pattern, flow_expr}

    clause_flow_return =
      {:clause, line, [{:atom, line, :return}], [],
       [{:tuple, line, [{:atom, line, :__return__}, {:var, line, flow_payload}]}]}

    clause_flow_break =
      {:clause, line, [{:atom, line, :break}], [], [{:atom, line, :__break__}]}

    clause_flow_ok = {:clause, line, [{:atom, line, :ok}], [], [{:atom, line, :ok}]}
    flow_result_case = {:case, line, {:var, line, flow_tag}, [clause_flow_return, clause_flow_break, clause_flow_ok]}

    {result_expr, counter} = sequence_expr(match_expr, flow_result_case, counter)

    # Update env with new variable names
    new_env = Enum.reduce(result_vars, env, fn {name, var}, acc ->
      Map.put(acc, name, var)
    end)

    {result_expr, new_env, counter}
  end

  defp if_flow_expr(cond, then_block, else_branch, env, counter, assigned_vars) do
    line = 1
    cond_form = expr_form(line, cond, env)
    fallback = build_return_tuple(line, assigned_vars, env, env)

    then_stmts =
      case then_block do
        {:block, %{stmts: stmts}} -> stmts
        _ -> []
      end

    {_then_body_expr, then_env, _counter_tmp} = stmt_expr_tree(then_stmts, env, counter)
    then_return_tuple = build_return_tuple(line, assigned_vars, then_env, env)
    {then_expr, _then_env, counter} =
      stmt_expr_tree_with_final(then_stmts, env, counter, then_return_tuple)
    {then_flow_expr, counter} = wrap_flow_expr(then_expr, fallback, line, counter)

    {else_flow_expr, counter} =
      case else_branch do
        {:else_block, %{block: {:block, %{stmts: else_stmts}}}} ->
          {_else_body_expr, else_env, _counter_tmp} = stmt_expr_tree(else_stmts, env, counter)
          else_return_tuple = build_return_tuple(line, assigned_vars, else_env, env)
          {else_expr, _else_env, counter} =
            stmt_expr_tree_with_final(else_stmts, env, counter, else_return_tuple)
          wrap_flow_expr(else_expr, fallback, line, counter)

        nil ->
          flow_expr =
            {:tuple, line, [{:atom, line, :__flow__}, {:atom, line, :ok}, {:atom, line, :ok}, fallback]}
          {flow_expr, counter}

        {:else_if, %{if: inner_if}} ->
          {:if_stmt, %{cond: inner_cond, then_block: inner_then, else_branch: inner_else}} = inner_if
          if_flow_expr(inner_cond, inner_then, inner_else, env, counter, assigned_vars)
      end

    clause_true = {:clause, line, [{:atom, line, true}], [], [then_flow_expr]}
    clause_false = {:clause, line, [{:atom, line, false}], [], [else_flow_expr]}
    {{:case, line, cond_form, [clause_true, clause_false]}, counter}
  end

  defp wrap_flow_expr(expr, fallback, line, counter) do
    {ok_var, counter} = fresh_var("if_value", counter)
    ret_var = internal_var("return_value")
    return_tuple = {:tuple, line, [{:atom, line, :__return__}, {:var, line, ret_var}]}
    break_var = internal_var("break_values")

    flow_tuple_ok =
      {:tuple, line,
       [
         {:atom, line, :__flow__},
         {:atom, line, :ok},
         {:atom, line, :ok},
         {:var, line, ok_var}
       ]}

    flow_tuple_return =
      {:tuple, line,
       [
         {:atom, line, :__flow__},
         {:atom, line, :return},
         {:var, line, ret_var},
         fallback
       ]}

    flow_tuple_break =
      {:tuple, line,
       [
         {:atom, line, :__flow__},
         {:atom, line, :break},
         {:atom, line, :ok},
         fallback
       ]}

    clause_return = {:clause, line, [return_tuple], [], [flow_tuple_return]}
    clause_break_tuple =
      {:clause, line, [{:tuple, line, [{:atom, line, :__break__}, {:var, line, break_var}]}], [],
       [{:tuple, line, [{:atom, line, :__flow__}, {:atom, line, :break}, {:atom, line, :ok}, {:var, line, break_var}]}]}
    clause_break = {:clause, line, [{:atom, line, :__break__}], [], [flow_tuple_break]}
    clause_ok = {:clause, line, [{:var, line, ok_var}], [], [flow_tuple_ok]}
    {{:case, line, expr, [clause_return, clause_break_tuple, clause_break, clause_ok]}, counter}
  end

  # Build a tuple expression with current values of variables
  # For each assigned_var, use the value from current_env if it was modified,
  # otherwise use the value from original_env
  defp build_return_tuple(line, var_names, current_env, original_env) do
    elements = Enum.map(var_names, fn name ->
      # Get the current var name from current_env
      current_var = Map.get(current_env, name)
      # Get the original var name from original_env
      original_var = Map.get(original_env, name)
      
      # Use the current value if it exists, otherwise use original
      case current_var || original_var do
        nil -> {:atom, line, :undefined}
        var -> {:var, line, var}
      end
    end)
    case elements do
      [] -> {:atom, line, :ok}
      [single] -> single
      _ -> {:tuple, line, elements}
    end
  end

  # Build a tuple pattern for matching
  defp build_pattern_tuple(line, var_pairs) do
    elements = Enum.map(var_pairs, fn {_name, var} ->
      {:var, line, var}
    end)
    case elements do
      [] -> {:atom, line, :ok}
      [single] -> single
      _ -> {:tuple, line, elements}
    end
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

  @spec for_expr(
          binary(),
          BeamLang.AST.expr(),
          [BeamLang.AST.stmt()],
          map(),
          non_neg_integer(),
          BeamLang.AST.type_name()
        ) ::
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

    clause_nil = {:clause, line, [{nil, line}], [], [{:atom, line, :ok}]}

    clause_cons =
      {:clause, line, [{:cons, line, {:var, line, head_var}, {:var, line, rest_var}}], [],
       [body_case]}

    fun_expr = {:named_fun, line, fun_name, [clause_nil, clause_cons]}
    match_fun = {:match, line, {:var, line, fun_var}, fun_expr}

    collection_form =
      cond do
        iterator_type?(collection_type) ->
          # Iterator: access .data field
          expr_form(line, {:field, %{target: collection, name: "data"}}, env)

        list_type?(collection_type) ->
          # List: access .data field
          expr_form(line, {:field, %{target: collection, name: "data"}}, env)

        range_type?(collection_type) ->
          # Range type: call iter() then access .data field
          range_iter_form(line, collection, env)

        true ->
          # Raw list or other iterable
          expr_form(line, collection, env)
      end

    call_fun = {:call, line, {:var, line, fun_var}, [collection_form]}
    {{:block, line, [match_fun, call_fun]}, env, counter}
  end

  defp range_type?(type) do
    case type do
      {:named, "Range"} -> true
      _ -> false
    end
  end

  defp range_iter_form(line, collection, env) do
    # Generate: collection->iter()->data which becomes range_iter(collection).data
    iter_call = {:method_call, %{target: collection, name: "iter", args: [], span: BeamLang.Span.new("<gen>", 0, 0)}}
    expr_form(line, {:field, %{target: iter_call, name: "data"}}, env)
  end

  @spec loop_fun_expr(
          BeamLang.AST.expr() | :always_true,
          [BeamLang.AST.stmt()],
          map(),
          non_neg_integer(),
          atom()
        ) ::
          {tuple(), map(), non_neg_integer()}
  defp loop_fun_expr(cond, body_stmts, env, counter, _kind) do
    line = 1
    {fun_var, counter} = fresh_var("loop", counter)
    fun_name = fun_var

    # Find all mutable variables that are assigned in the loop body
    assigned_vars = find_assigned_vars(body_stmts)
    # Get the current env mappings for these vars (their current Erlang variable names)
    loop_vars = for var_name <- assigned_vars, Map.has_key?(env, var_name), do: var_name

    # Create fresh parameter names for loop function
    {param_vars, counter} =
      Enum.reduce(loop_vars, {[], counter}, fn name, {acc, cnt} ->
        {pvar, cnt} = fresh_var("#{name}_loop", cnt)
        {acc ++ [{name, pvar}], cnt}
      end)

    # Build env for loop body: map each name to its parameter variable
    body_env =
      Enum.reduce(param_vars, env, fn {name, pvar}, acc ->
        Map.put(acc, name, pvar)
      end)
      |> Map.put(:__loop_vars__, param_vars)

    # Generate body statements as a list of forms (no return-case wrapping)
    {body_forms, env_after_body, counter} = loop_body_forms(body_stmts, body_env, counter)

    # Build the recursive call with updated variables
    continue_args =
      Enum.map(param_vars, fn {name, _pvar} ->
        updated_var = Map.get(env_after_body, name)
        {:var, line, updated_var}
      end)

    continue_call = {:call, line, {:var, line, fun_name}, continue_args}

    # Combine body forms with continue call, handling return/break
    body_with_continue = build_loop_body_with_continue(body_forms, continue_call, env_after_body, param_vars, counter)

    # Build condition - must use parameter variables
    cond_form =
      case cond do
        :always_true -> {:atom, line, true}
        _ -> expr_form(line, cond, body_env)
      end

    # When condition is false, return tuple of final variable values
    final_values_tuple = build_vars_tuple(line, param_vars, body_env)

    clause_true = {:clause, line, [{:atom, line, true}], [], [body_with_continue]}
    clause_false = {:clause, line, [{:atom, line, false}], [], [final_values_tuple]}
    cond_case = {:case, line, cond_form, [clause_true, clause_false]}

    # Function parameters
    fun_params = Enum.map(param_vars, fn {_name, pvar} -> {:var, line, pvar} end)
    fun_expr = {:named_fun, line, fun_name, [{:clause, line, fun_params, [], [cond_case]}]}
    match_fun = {:match, line, {:var, line, fun_var}, fun_expr}

    # Initial call with current variable values
    initial_args =
      Enum.map(loop_vars, fn name ->
        current_var = Map.get(env, name)
        {:var, line, current_var}
      end)

    call_fun = {:call, line, {:var, line, fun_var}, initial_args}

    # Create fresh variables for the results and update env
    {result_vars, new_env, counter} =
      Enum.reduce(loop_vars, {[], env, counter}, fn name, {acc_vars, acc_env, cnt} ->
        {rvar, cnt} = fresh_var("#{name}_result", cnt)
        {acc_vars ++ [{name, rvar}], Map.put(acc_env, name, rvar), cnt}
      end)

    if length(result_vars) == 0 do
      # No mutable vars - simple call
      {{:block, line, [match_fun, call_fun]}, env, counter}
    else
      # Match the returned tuple to get final values
      result_pattern = build_vars_tuple(line, result_vars, %{})
      match_result = {:match, line, result_pattern, call_fun}
      {{:block, line, [match_fun, match_result]}, new_env, counter}
    end
  end

  # Generate loop body forms as a list without return-case wrapping
  defp loop_body_forms([], env, counter), do: {[], env, counter}
  defp loop_body_forms([stmt | rest], env, counter) do
    {form, env, counter} = loop_stmt_form(stmt, env, counter)
    {rest_forms, env, counter} = loop_body_forms(rest, env, counter)
    {[form | rest_forms], env, counter}
  end

  # Convert statement to form for loop body (simpler than stmt_form, no return wrapping)
  defp loop_stmt_form({:let, %{name: name, expr: expr}}, env, counter) do
    {var, counter} = fresh_var(name, counter)
    form = match_form(1, var, expr, env)
    {form, Map.put(env, name, var), counter}
  end

  defp loop_stmt_form({:assign, %{target: target, expr: expr}}, env, counter) do
    case assignment_form(1, target, expr, env, counter) do
      {:ok, form, new_env, new_counter} -> {form, new_env, new_counter}
      {:error, _} -> {{:atom, 1, :ok}, env, counter}
    end
  end

  defp loop_stmt_form({:expr, %{expr: expr}}, env, counter) do
    {expr_form(1, expr, env), env, counter}
  end

  defp loop_stmt_form({:return, %{expr: nil}}, env, counter) do
    {return_marker({:atom, 1, :ok}), env, counter}
  end

  defp loop_stmt_form({:return, %{expr: expr}}, env, counter) do
    {return_marker(expr_form(1, expr, env)), env, counter}
  end

  defp loop_stmt_form({:break, %{}}, env, counter) do
    case Map.get(env, :__loop_vars__) do
      nil ->
        {{:atom, 1, :__break__}, env, counter}

      vars ->
        break_values = build_vars_tuple(1, vars, env)
        {{:tuple, 1, [{:atom, 1, :__break__}, break_values]}, env, counter}
    end
  end

  defp loop_stmt_form({:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}}, env, counter) do
    then_stmts =
      case then_block do
        {:block, %{stmts: stmts}} -> stmts
        _ -> []
      end

    {else_stmts, else_if_vars} =
      case else_branch do
        {:else_block, %{block: {:block, %{stmts: stmts}}}} -> {stmts, []}
        {:else_if, %{if: if_stmt}} -> {[], find_assigned_in_stmt(if_stmt)}
        nil -> {[], []}
      end

    assigned_vars =
      (find_assigned_vars(then_stmts) ++ find_assigned_vars(else_stmts) ++ else_if_vars)
      |> Enum.uniq()
      |> Enum.filter(&Map.has_key?(env, &1))

    if Enum.empty?(assigned_vars) do
      # For if in loop body, we need to handle it specially
      {then_forms, _then_env, counter} =
        case then_block do
          {:block, %{stmts: stmts}} -> loop_body_forms(stmts, env, counter)
          _ -> {[], env, counter}
        end

      {else_forms, _else_env, counter} =
        case else_branch do
          {:else_block, %{block: {:block, %{stmts: stmts}}}} -> loop_body_forms(stmts, env, counter)
          nil -> {[{:atom, 1, :ok}], env, counter}
          _ -> {[{:atom, 1, :ok}], env, counter}
        end

      cond_form = expr_form(1, cond, env)
      then_expr = if length(then_forms) == 1, do: hd(then_forms), else: {:block, 1, then_forms}
      else_expr = if length(else_forms) == 1, do: hd(else_forms), else: {:block, 1, else_forms}

      clause_true = {:clause, 1, [{:atom, 1, true}], [], [then_expr]}
      clause_false = {:clause, 1, [{:atom, 1, false}], [], [else_expr]}
      {{:case, 1, cond_form, [clause_true, clause_false]}, env, counter}
    else
      if_stmt_expr_with_propagation(cond, then_block, else_branch, env, counter, assigned_vars)
    end
  end

  defp loop_stmt_form(stmt, env, counter) do
    # Fallback to regular stmt_form
    stmt_form(stmt, env, counter)
  end

  # Build the loop body with continue call at the end
  defp build_loop_body_with_continue(body_forms, continue_call, env_after_body, param_vars, _counter) do
    line = 1

    if Enum.empty?(body_forms) do
      continue_call
    else
      # Check if any form might be a return or break
      # If so, we need to wrap in case to catch it
      has_control_flow = Enum.any?(body_forms, &has_return_or_break?/1)

      if has_control_flow do
        # Wrap in case to handle return/break
        body_expr = if length(body_forms) == 1, do: hd(body_forms), else: {:block, line, body_forms}
        ret_var = internal_var("return_value")
        return_tuple = {:tuple, line, [{:atom, line, :__return__}, {:var, line, ret_var}]}
        clause_return = {:clause, line, [return_tuple], [], [return_tuple]}

        break_values = build_vars_tuple(line, param_vars, env_after_body)
        break_var = internal_var("break_values")
        clause_break =
          {:clause, line, [{:atom, line, :__break__}], [], [break_values]}
        clause_break_tuple =
          {:clause, line, [{:tuple, line, [{:atom, line, :__break__}, {:var, line, break_var}]}], [], [{:var, line, break_var}]}
        clause_continue = {:clause, line, [{:var, line, :_}], [], [continue_call]}
        {:case, line, body_expr, [clause_return, clause_break_tuple, clause_break, clause_continue]}
      else
        # Simple case: just sequence forms and add continue call
        {:block, line, body_forms ++ [continue_call]}
      end
    end
  end

  defp has_return_or_break?({:tuple, _, [{:atom, _, :__return__}, _]}), do: true
  defp has_return_or_break?({:atom, _, :__break__}), do: true
  defp has_return_or_break?({:tuple, _, [{:atom, _, :__break__}, _]}), do: true
  defp has_return_or_break?({:case, _, _, clauses}) do
    Enum.any?(clauses, fn {:clause, _, _, _, body} ->
      Enum.any?(body, &has_return_or_break?/1)
    end)
  end
  defp has_return_or_break?({:block, _, exprs}), do: Enum.any?(exprs, &has_return_or_break?/1)
  defp has_return_or_break?(_), do: false

  # Build a tuple of variables
  defp build_vars_tuple(line, vars, _env) when length(vars) == 0 do
    {:atom, line, :ok}
  end
  defp build_vars_tuple(line, vars, env) when length(vars) == 1 do
    [{name, var}] = vars
    actual_var = Map.get(env, name, var)
    {:var, line, actual_var}
  end
  defp build_vars_tuple(line, vars, env) do
    elements = Enum.map(vars, fn {name, var} ->
      actual_var = Map.get(env, name, var)
      {:var, line, actual_var}
    end)
    {:tuple, line, elements}
  end

  # Find variable names that are assigned in statements
  defp find_assigned_vars(stmts) do
    stmts
    |> Enum.flat_map(&find_assigned_in_stmt/1)
    |> Enum.uniq()
  end

  defp find_assigned_in_stmt({:assign, %{target: {:identifier, %{name: name}}}}), do: [name]
  defp find_assigned_in_stmt({:assign, %{target: {:field, %{target: {:identifier, %{name: name}}}}}}), do: [name]
  defp find_assigned_in_stmt({:if_stmt, %{then_block: {:block, %{stmts: then_stmts}}, else_branch: else_branch}}) do
    then_vars = find_assigned_vars(then_stmts)
    else_vars = case else_branch do
      {:else_block, %{block: {:block, %{stmts: else_stmts}}}} -> find_assigned_vars(else_stmts)
      {:else_if, %{if: if_stmt}} -> find_assigned_in_stmt(if_stmt)
      nil -> []
    end
    then_vars ++ else_vars
  end
  defp find_assigned_in_stmt({:while, %{body: {:block, %{stmts: body_stmts}}}}), do: find_assigned_vars(body_stmts)
  defp find_assigned_in_stmt({:loop, %{body: {:block, %{stmts: body_stmts}}}}), do: find_assigned_vars(body_stmts)
  defp find_assigned_in_stmt({:for, %{body: {:block, %{stmts: body_stmts}}}}), do: find_assigned_vars(body_stmts)
  defp find_assigned_in_stmt(_), do: []

  @spec break_case(tuple(), tuple(), non_neg_integer()) :: {tuple(), non_neg_integer()}
  defp break_case(body_expr, continue_expr, counter) do
    line = 1
    ret_var = internal_var("return_value")
    return_tuple = {:tuple, line, [{:atom, line, :__return__}, {:var, line, ret_var}]}
    clause_return = {:clause, line, [return_tuple], [], [return_tuple]}
    break_var = internal_var("break_values")
    clause_break = {:clause, line, [{:atom, line, :__break__}], [], [{:atom, line, :ok}]}
    clause_break_tuple =
      {:clause, line, [{:tuple, line, [{:atom, line, :__break__}, {:var, line, break_var}]}], [], [{:var, line, break_var}]}
    clause_continue = {:clause, line, [{:var, line, :_}], [], [continue_expr]}
    {{:case, line, body_expr, [clause_return, clause_break_tuple, clause_break, clause_continue]}, counter}
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
    break_var = internal_var("break_values")
    clause_break = {:clause, line, [{:atom, line, :__break__}], [], [{:atom, line, :__break__}]}
    clause_break_tuple =
      {:clause, line, [{:tuple, line, [{:atom, line, :__break__}, {:var, line, break_var}]}], [], [{:tuple, line, [{:atom, line, :__break__}, {:var, line, break_var}]}]}
    clause_continue = {:clause, line, [{:var, line, :_}], [], [continue_expr]}
    {{:case, line, expr, [clause_return, clause_break_tuple, clause_break, clause_continue]}, counter}
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

  defp list_type?({:generic, {:named, "List"}, _}), do: true
  defp list_type?({:named, "List"}), do: true
  defp list_type?(_), do: false

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
    Enum.reduce(params, {[], env, counter}, fn param, {acc, env_acc, counter_acc} ->
      case param do
        # Regular named parameter
        %{name: name} ->
          {var, counter_acc} = fresh_var(name, counter_acc)
          param_form = {:var, line, var}
          {acc ++ [param_form], Map.put(env_acc, name, var), counter_acc}

        # Pattern parameter (struct or tuple destructuring)
        %{pattern: pattern} ->
          {param_form, new_env, counter_acc} = pattern_param_form(pattern, line, env_acc, counter_acc)
          {acc ++ [param_form], new_env, counter_acc}
      end
    end)
  end

  # Convert a pattern parameter to Erlang form and extract bindings
  defp pattern_param_form({:struct_pattern, %{name: type_name, fields: fields}}, line, env, counter) do
    # Create a map pattern that matches on __beamlang_type__ and extracts fields
    {field_pairs, new_env, counter} =
      Enum.reduce(fields, {[], env, counter}, fn field, {pairs_acc, env_acc, c} ->
        name = field.name
        {var, c} = fresh_var(name, c)
        pair = {:map_field_exact, line, {:atom, line, String.to_atom(name)}, {:var, line, var}}
        {pairs_acc ++ [pair], Map.put(env_acc, name, var), c}
      end)

    # Use __beamlang_type__ as a charlist (same as in pattern_form for match)
    struct_pair = {:map_field_exact, line, {:atom, line, :__beamlang_type__}, {:string, line, to_charlist(type_name)}}
    pattern_form = {:map, line, [struct_pair | field_pairs]}
    {pattern_form, new_env, counter}
  end

  defp pattern_param_form({:tuple_pattern, %{elements: elements}}, line, env, counter) do
    {elem_forms, new_env, counter} =
      Enum.reduce(elements, {[], env, counter}, fn elem, {forms_acc, env_acc, c} ->
        # Handle both keyword list format (from parser) and tuple format
        {tag, info} = case elem do
          {tag, info} when is_atom(tag) -> {tag, info}
          other -> other
        end

        case tag do
          :pat_identifier ->
            name = info.name
            {var, c} = fresh_var(name, c)
            {forms_acc ++ [{:var, line, var}], Map.put(env_acc, name, var), c}

          :var_pattern ->
            name = info.name
            {var, c} = fresh_var(name, c)
            {forms_acc ++ [{:var, line, var}], Map.put(env_acc, name, var), c}

          :wildcard ->
            {forms_acc ++ [{:var, line, :_}], env_acc, c}

          # Nested patterns could be handled here
          _ ->
            {forms_acc ++ [{:var, line, :_}], env_acc, c}
        end
      end)

    pattern_form = {:tuple, line, elem_forms}
    {pattern_form, new_env, counter}
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

          {:ok,
           {:call, line, {:remote, line, {:atom, line, mod_atom}, {:atom, line, fun_atom}},
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
  defp type_label({:type_var, name}) when is_binary(name), do: name

  defp type_label({:result, ok_type, err_type}),
    do: "#{type_label(ok_type)}!#{type_label(err_type)}"

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

  defp pattern_form(line, {:enum_pattern, %{enum_name: enum_name, variant: variant, fields: fields}}, counter) do
    # Generate: %{__enum__: "EnumName", __variant__: "Variant", field1: Pat1, ...}
    base_entries = [
      {:map_field_exact, line, {:atom, line, :__enum__}, {:bin, line, [{:bin_element, line, {:string, line, String.to_charlist(enum_name)}, :default, :default}]}},
      {:map_field_exact, line, {:atom, line, :__variant__}, {:bin, line, [{:bin_element, line, {:string, line, String.to_charlist(variant)}, :default, :default}]}}
    ]

    {field_entries, env, counter} =
      Enum.reduce(fields, {[], %{}, counter}, fn field, {acc_entries, acc_env, acc_counter} ->
        {pat_form, pat_env, new_counter} = pattern_form(line, field.pattern, acc_counter)
        entry = {:map_field_exact, line, {:atom, line, String.to_atom(field.name)}, pat_form}
        {[entry | acc_entries], Map.merge(acc_env, pat_env), new_counter}
      end)

    {{:map, line, base_entries ++ Enum.reverse(field_entries)}, env, counter}
  end

  defp pattern_form(line, {:tuple_pattern, %{elements: elements}}, counter) do
    {elem_forms, env, counter} =
      Enum.reduce(elements, {[], %{}, counter}, fn elem, {acc_forms, acc_env, acc_counter} ->
        {pat_form, pat_env, new_counter} = pattern_form(line, elem, acc_counter)
        {[pat_form | acc_forms], Map.merge(acc_env, pat_env), new_counter}
      end)

    {{:tuple, line, Enum.reverse(elem_forms)}, env, counter}
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

  @spec build_interpolated_string(non_neg_integer(), [binary()], [tuple()]) :: tuple()
  defp build_interpolated_string(line, parts, expr_forms) do
    part_forms =
      Enum.map(parts, fn part ->
        {:string, line, String.to_charlist(part)}
      end)

    combined = interleave_with_conversion(line, part_forms, expr_forms)

    case combined do
      [] ->
        data = {:string, line, []}
        {:call, line, {:atom, line, :string_new}, [data]}

      [first | rest] ->
        Enum.reduce(rest, first, fn elem, acc ->
          fun_expr =
            {:call, line, {:remote, line, {:atom, line, :maps}, {:atom, line, :get}},
             [{:atom, line, :concat}, acc]}

          {:call, line, fun_expr, [acc, elem]}
        end)
    end
  end

  @spec interleave_with_conversion(non_neg_integer(), [tuple()], [tuple()]) :: [tuple()]
  defp interleave_with_conversion(line, part_forms, expr_forms) do
    part_forms
    |> Enum.zip(expr_forms ++ [nil])
    |> Enum.flat_map(fn {part, expr} ->
      part_string = {:call, line, {:atom, line, :string_new}, [part]}

      if expr do
        # Call stdlib to_string function which wraps any_to_string_data
        expr_string = {:call, line, {:atom, line, :to_string}, [expr]}
        [part_string, expr_string]
      else
        [part_string]
      end
    end)
  end

  # Helper functions for string type checking (moved here to keep expr_form clauses grouped)
  defp get_expr_type({_tag, %{type: type}}), do: type
  defp get_expr_type({:string, _}), do: :String
  defp get_expr_type(_), do: nil

  defp is_string_type?(:String), do: true
  defp is_string_type?({:named, "String"}), do: true
  defp is_string_type?(_), do: false

  defp ensure_string_form(line, expr, type, env) do
    if is_string_type?(type) do
      expr_form(line, expr, env)
    else
      # Convert to string using to_string
      {:call, line, {:atom, line, :to_string}, [expr_form(line, expr, env)]}
    end
  end

  # Helper functions for destructuring
  @spec destruct_struct_bindings([map()], tuple(), map(), non_neg_integer()) ::
          {[{atom(), atom()}], map(), non_neg_integer()}
  defp destruct_struct_bindings(fields, _expr_form, env, counter) do
    Enum.reduce(fields, {[], env, counter}, fn field, {bindings, env, counter} ->
      binding_name = field.binding || field.name
      field_atom = String.to_atom(field.name)
      {var, counter} = fresh_var(binding_name, counter)
      env = Map.put(env, binding_name, var)
      {[{field_atom, var} | bindings], env, counter}
    end)
  end

  @spec destruct_struct_form(non_neg_integer(), [{atom(), atom()}], tuple()) :: tuple()
  defp destruct_struct_form(line, bindings, expr_form) do
    # Generate: #{field1 := Var1, field2 := Var2} = expr
    entries = Enum.map(bindings, fn {field_atom, var} ->
      {:map_field_exact, line, {:atom, line, field_atom}, {:var, line, var}}
    end)
    {:match, line, {:map, line, entries}, expr_form}
  end

  @spec destruct_tuple_bindings([binary() | map()], map(), non_neg_integer()) ::
          {[atom()], map(), non_neg_integer()}
  defp destruct_tuple_bindings(elements, env, counter) do
    Enum.reduce(elements, {[], env, counter}, fn elem, {vars, env, counter} ->
      name = case elem do
        %{name: n} -> n
        n when is_binary(n) -> n
      end
      {var, counter} = fresh_var(name, counter)
      env = Map.put(env, name, var)
      {[var | vars], env, counter}
    end)
  end

  @spec destruct_tuple_form(non_neg_integer(), [atom()], tuple()) :: tuple()
  defp destruct_tuple_form(line, vars, expr_form) do
    # Generate: {Var1, Var2, ...} = expr
    var_forms = Enum.reverse(vars) |> Enum.map(fn var -> {:var, line, var} end)
    {:match, line, {:tuple, line, var_forms}, expr_form}
  end
end
