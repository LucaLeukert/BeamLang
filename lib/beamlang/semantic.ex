defmodule BeamLang.Semantic do
  @moduledoc """
  Performs basic semantic checks for the MVP.
  """

  @spec validate(BeamLang.AST.t()) :: {:ok, BeamLang.AST.t()} | {:error, [BeamLang.Error.t()]}
  def validate({:program, %{types: types, functions: functions}} = ast) when is_list(functions) do
    {:ok, func_table} = build_function_table(functions)
    {:ok, type_table} = build_type_table(types)

    errors =
      collect_main_error(functions) ++
        collect_type_errors(types, type_table) ++
        collect_function_errors(functions, func_table, type_table)

    case errors do
      [] -> {:ok, ast}
      _ -> {:error, errors}
    end
  end

  @spec require_main_exists([BeamLang.AST.func()]) :: :ok | {:error, [BeamLang.Error.t()]}
  defp require_main_exists(functions) do
    case Enum.find(functions, fn {:function, %{name: name}} -> name == "main" end) do
      nil ->
        span = BeamLang.Span.new("<source>", 0, 0)
        {:error, [BeamLang.Error.new(:type, "Missing required function 'main'.", span)]}

      _ ->
        :ok
    end
  end

  @spec typecheck_return(BeamLang.AST.type_name(), BeamLang.AST.block(), map(), map(), map()) ::
          :ok | {:error, [BeamLang.Error.t()]}
  defp typecheck_return(expected_type, {:block, %{stmts: stmts}} = block, func_table, type_table, env) do
    case List.last(stmts) do
      {:return, %{expr: nil, span: span}} ->
        if expected_type == :void do
          :ok
        else
          {:error, [BeamLang.Error.new(:type, "Non-void function must return a value.", span)]}
        end

      {:return, %{expr: expr}} ->
        case type_of_expr(expr, func_table, type_table, env, expected_type) do
          {:error, :unknown_function} ->
            {:error, [BeamLang.Error.new(:type, "Unknown function in return expression.", expr_span(expr))]}

          {:error, :wrong_arity} ->
            {:error, [BeamLang.Error.new(:type, "Function called with wrong number of arguments.", expr_span(expr))]}

          {:error, :unknown_variable} ->
            {:error, [BeamLang.Error.new(:type, "Unknown variable in return expression.", expr_span(expr))]}

          {:error, :unknown_type} ->
            {:error, [BeamLang.Error.new(:type, "Unknown type in return expression.", expr_span(expr))]}

          {:error, :missing_type_annotation} ->
            {:error, [BeamLang.Error.new(:type, "Struct literal requires a type annotation.", expr_span(expr))]}

          {:error, :not_a_struct} ->
            {:error, [BeamLang.Error.new(:type, "Field access requires a struct value.", expr_span(expr))]}

          {:error, {:unknown_field, name}} ->
            {:error, [BeamLang.Error.new(:type, "Unknown field '#{name}'.", expr_span(expr))]}

          {:error, {:struct, errors}} ->
            {:error, errors}

          {:error, {:match, errors}} ->
            {:error, errors}

          {:error, {:call, errors}} ->
            {:error, errors}

          {:ok, inferred} ->
            if type_compatible?(expected_type, inferred) do
              :ok
            else
              {:error,
               [
                 BeamLang.Error.new(
                   :type,
                   "Return type mismatch. Expected #{type_label(expected_type)}, got #{type_label(inferred)}.",
                   expr_span(expr)
                 )
               ]}
            end
        end

      _ ->
        if expected_type == :void do
          :ok
        else
          if block_returns?(block) do
            :ok
          else
            {:error,
             [BeamLang.Error.new(:type, "Missing return statement.", block_span(stmts))]}
          end
        end
    end
  end

  @spec require_main_i32(binary(), BeamLang.AST.type_name()) :: :ok | {:error, [BeamLang.Error.t()]}
  defp require_main_i32("main", :i32), do: :ok

  defp require_main_i32("main", other),
    do:
      {:error,
       [
         BeamLang.Error.new(
           :type,
           "main must return i32, got #{type_label(other)}.",
           BeamLang.Span.new("<source>", 0, 0)
         )
       ]}

  defp require_main_i32(_name, _type), do: :ok

  @spec type_of_expr(BeamLang.AST.expr(), map(), map(), map(), BeamLang.AST.type_name() | nil) ::
          {:ok, BeamLang.AST.type_name()}
          | {:error,
             :unknown_function
             | :wrong_arity
             | :unknown_variable
             | :unknown_type
             | :missing_type_annotation
             | {:struct, [BeamLang.Error.t()]}
             | {:match, [BeamLang.Error.t()]}
             | {:call, [BeamLang.Error.t()]}
             | :invalid_binary_op
             | {:unknown_field, binary()}
             | :not_a_struct}
  defp type_of_expr({:integer, %{value: _value}}, _func_table, _type_table, _env, _expected), do: {:ok, :integer}
  defp type_of_expr({:float, %{value: _value}}, _func_table, _type_table, _env, _expected), do: {:ok, :float}
  defp type_of_expr({:string, %{value: _value}}, _func_table, _type_table, _env, _expected), do: {:ok, :String}
  defp type_of_expr({:bool, %{value: _value}}, _func_table, _type_table, _env, _expected), do: {:ok, :bool}

  defp type_of_expr({:call, %{name: name, args: args}}, func_table, type_table, env, _expected) do
    case Map.fetch(func_table, name) do
      {:ok, {param_types, return_type}} ->
        if length(param_types) != length(args) do
          {:error, :wrong_arity}
        else
          errors =
            Enum.zip(args, param_types)
            |> Enum.flat_map(fn {arg, expected_type} ->
              case type_of_expr(arg, func_table, type_table, env, expected_type) do
                {:ok, inferred} ->
                  if type_compatible?(expected_type, inferred) do
                    []
                  else
                    [
                      BeamLang.Error.new(
                        :type,
                        "Argument type mismatch. Expected #{type_label(expected_type)}, got #{type_label(inferred)}.",
                        expr_span(arg)
                      )
                    ]
                  end

                {:error, reason} ->
                  expr_error(reason, arg)
              end
            end)

          if errors == [], do: {:ok, return_type}, else: {:error, {:call, errors}}
        end

      :error ->
        {:error, :unknown_function}
    end
  end

  defp type_of_expr({:identifier, %{name: name}}, _func_table, _type_table, env, _expected) do
    case Map.fetch(env, name) do
      {:ok, %{type: type}} -> {:ok, type}
      :error -> {:error, :unknown_variable}
    end
  end

  defp type_of_expr({:struct, %{fields: fields}}, func_table, type_table, env, expected) do
    case expected do
      {:named, type_name} ->
        case Map.fetch(type_table, type_name) do
          {:ok, type_fields} ->
            errors = validate_struct_fields(fields, type_fields, func_table, type_table, env, type_name)
            if errors == [], do: {:ok, {:named, type_name}}, else: {:error, {:struct, errors}}

          :error ->
            {:error, :unknown_type}
        end

      nil ->
        {:error, :missing_type_annotation}

      _ ->
        {:error, :unknown_type}
    end
  end

  defp type_of_expr({:field, %{target: target, name: name}}, func_table, type_table, env, _expected) do
    case type_of_expr(target, func_table, type_table, env, nil) do
      {:ok, {:named, type_name}} ->
        case Map.fetch(type_table, type_name) do
          {:ok, field_map} ->
            case Map.fetch(field_map, name) do
              {:ok, field_type} -> {:ok, field_type}
              :error -> {:error, {:unknown_field, name}}
            end

          :error ->
            {:error, :unknown_type}
        end

      {:ok, _} ->
        {:error, :not_a_struct}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp type_of_expr({:block_expr, %{block: block}}, func_table, type_table, env, _expected) do
    {type, errors} = block_expr_type(block, func_table, type_table, env)
    if errors == [], do: {:ok, type}, else: {:error, {:match, errors}}
  end

  defp type_of_expr({:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch}}, func_table, type_table, env, _expected) do
    errors = condition_errors(cond, func_table, type_table, env)
    {then_type, then_errors} = block_expr_type(then_block, func_table, type_table, env)
    {else_type, else_errors} = else_branch_type(else_branch, func_table, type_table, env)
    errors = errors ++ then_errors ++ else_errors

    if errors != [] do
      {:error, {:match, errors}}
    else
      if type_compatible?(then_type, else_type) do
        {:ok, then_type}
      else
        {:error,
         {:match,
          [
            BeamLang.Error.new(
              :type,
              "If expression type mismatch. Expected #{type_label(then_type)}, got #{type_label(else_type)}.",
              expr_span(cond)
            )
          ]}}
      end
    end
  end

  defp type_of_expr({:match, %{expr: expr, cases: cases}}, func_table, type_table, env, _expected) do
    case type_of_expr(expr, func_table, type_table, env, nil) do
      {:ok, match_type} ->
        {case_types, errors} =
          Enum.reduce(cases, {[], []}, fn %{pattern: pattern, guard: guard, body: body}, {types, errs} ->
            {bindings, pattern_errors} = pattern_bindings(pattern, match_type, type_table)
            case_env = Map.merge(env, bindings)
            guard_errors = guard_errors(guard, func_table, type_table, case_env)
            {body_type, body_errors} = case_body_type(body, func_table, type_table, case_env)
            {[{body_type, expr_span(body)} | types], errs ++ pattern_errors ++ guard_errors ++ body_errors}
          end)

        case_types = Enum.reverse(case_types)

        if errors != [] do
          {:error, {:match, errors}}
        else
          case case_types do
            [] ->
              {:error,
               {:match,
                [BeamLang.Error.new(:type, "Match expression must have at least one case.", BeamLang.Span.new("<source>", 0, 0))]}}

            _ ->
              expected = select_match_type(case_types)

              case mismatch_type(expected, case_types) do
                :ok ->
                  {:ok, expected}

                {:error, mismatches} ->
                  {:error, {:match, mismatches}}
              end
          end
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp type_of_expr({:binary, %{op: _op, left: left, right: right}}, func_table, type_table, env, _expected) do
    with {:ok, left_type} <- type_of_expr(left, func_table, type_table, env, nil),
         {:ok, right_type} <- type_of_expr(right, func_table, type_table, env, nil) do
      if comparable_types?(left_type, right_type) do
        {:ok, :bool}
      else
        {:error, :invalid_binary_op}
      end
    end
  end

  @spec validate_function(BeamLang.AST.func(), map(), map()) :: [BeamLang.Error.t()]
  defp validate_function({:function, %{name: name, params: params, return_type: return_type, body: body}}, func_table, type_table) do
    errors = []

    param_env =
      Enum.reduce(params, %{}, fn %{name: param_name, type: param_type}, acc ->
        Map.put(acc, param_name, %{type: param_type, mutable: false})
      end)

    {env, stmt_errors} =
      case validate_statements(body, func_table, type_table, param_env) do
        {:ok, env, errs} -> {env, errs}
      end

    errors = errors ++ stmt_errors

    errors =
      case typecheck_return(return_type, body, func_table, type_table, env) do
        :ok -> errors
        {:error, errs} -> errors ++ errs
      end

    errors =
      case require_main_i32(name, return_type) do
        :ok -> errors
        {:error, errs} -> errors ++ errs
      end

    errors
  end

  @spec validate_statements(BeamLang.AST.block(), map(), map(), map()) ::
          {:ok, map(), [BeamLang.Error.t()]}
  defp validate_statements(block, func_table, type_table, env) do
    validate_statements(block, func_table, type_table, env, 0)
  end

  @spec validate_statements(BeamLang.AST.block(), map(), map(), map(), non_neg_integer()) ::
          {:ok, map(), [BeamLang.Error.t()]}
  defp validate_statements({:block, %{stmts: stmts}}, func_table, type_table, env, loop_depth) do
    Enum.reduce_while(stmts, {:ok, env, []}, fn stmt, {:ok, acc_env, errors} ->
      case stmt do
        {:break, %{span: span}} ->
          if loop_depth == 0 do
            {:cont, {:ok, acc_env, [BeamLang.Error.new(:type, "break used outside of loop.", span) | errors]}}
          else
            {:cont, {:ok, acc_env, errors}}
          end

        {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}} ->
          errors = errors ++ condition_errors(cond, func_table, type_table, acc_env)
          {:ok, _env_then, then_errors} = validate_statements(then_block, func_table, type_table, acc_env, loop_depth)
          errors = then_errors ++ errors
          {errors, _} = else_errors(else_branch, func_table, type_table, acc_env, loop_depth, errors)
          {:cont, {:ok, acc_env, errors}}

        {:while, %{cond: cond, body: body}} ->
          errors = errors ++ condition_errors(cond, func_table, type_table, acc_env)
          {:ok, _env_body, body_errors} = validate_statements(body, func_table, type_table, acc_env, loop_depth + 1)
          {:cont, {:ok, acc_env, body_errors ++ errors}}

        {:loop, %{body: body}} ->
          {:ok, _env_body, body_errors} = validate_statements(body, func_table, type_table, acc_env, loop_depth + 1)
          {:cont, {:ok, acc_env, body_errors ++ errors}}

        {:for, %{name: name, collection: collection, body: body, span: span}} ->
          errors = errors ++ iterator_errors(collection, func_table, type_table, acc_env, span)
          item_type = iterator_item_type(collection, func_table, type_table, acc_env)
          loop_env = Map.put(acc_env, name, %{type: item_type, mutable: false})
          {:ok, _env_body, body_errors} = validate_statements(body, func_table, type_table, loop_env, loop_depth + 1)
          {:cont, {:ok, acc_env, body_errors ++ errors}}

        {:guard, %{cond: cond, else_block: else_block}} ->
          errors =
            case type_of_expr(cond, func_table, type_table, acc_env, :bool) do
              {:ok, :bool} ->
                errors

              {:ok, type} ->
                [
                  BeamLang.Error.new(
                    :type,
                    "Guard condition must be bool, got #{type_label(type)}.",
                    expr_span(cond)
                  )
                  | errors
                ]

              {:error, reason} ->
                expr_error(reason, cond) ++ errors
            end

          {:ok, _env, else_errors} = validate_statements(else_block, func_table, type_table, acc_env, loop_depth)
          errors = else_errors ++ errors

          errors =
            if guard_has_return?(else_block) do
              errors
            else
              [BeamLang.Error.new(:type, "Guard else block must end with return.", block_span(else_block)) | errors]
            end

          {:cont, {:ok, acc_env, errors}}

        {:let, %{name: name, expr: expr, mutable: mutable, type: declared_type}} ->
          case type_of_expr(expr, func_table, type_table, acc_env, declared_type) do
            {:error, :unknown_function} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown function in let expression.", expr_span(expr)) | errors]}}

            {:error, :wrong_arity} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(:type, "Function called with wrong number of arguments.", expr_span(expr))
                  | errors
                ]}}

            {:error, :unknown_variable} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown variable in let expression.", expr_span(expr)) | errors]}}

            {:error, :unknown_type} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown type in let expression.", expr_span(expr)) | errors]}}

            {:error, :missing_type_annotation} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Struct literal requires a type annotation.", expr_span(expr)) | errors]}}

            {:error, :not_a_struct} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Field access requires a struct value.", expr_span(expr)) | errors]}}

            {:error, {:unknown_field, field}} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown field '#{field}'.", expr_span(expr)) | errors]}}

            {:error, {:struct, errs}} ->
              {:cont, {:ok, acc_env, errs ++ errors}}

            {:error, {:match, errs}} ->
              {:cont, {:ok, acc_env, errs ++ errors}}

            {:error, {:call, errs}} ->
              {:cont, {:ok, acc_env, errs ++ errors}}

            {:ok, inferred} ->
              if Map.has_key?(acc_env, name) do
                {:cont,
                 {:ok, acc_env,
                  [
                    BeamLang.Error.new(:type, "Variable '#{name}' is already defined.", stmt_span(stmt))
                    | errors
                  ]}}
              else
                {:cont, {:ok, Map.put(acc_env, name, %{type: inferred, mutable: mutable}), errors}}
              end
          end

        {:assign, %{target: target, expr: expr}} ->
          case assignment_target(target, acc_env) do
            {:error, err} ->
              {:cont, {:ok, acc_env, [err | errors]}}

            {:ok, %{name: name, type: _declared, mutable: false}} ->
              {:cont,
               {:ok, acc_env,
                [BeamLang.Error.new(:type, "Cannot assign to immutable variable '#{name}'.", stmt_span(stmt)) | errors]}}

            {:ok, %{name: _name, type: declared, mutable: true, field: nil}} ->
              case type_of_expr(expr, func_table, type_table, acc_env, declared) do
                {:error, :unknown_function} ->
                  {:cont,
                   {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown function in assignment.", expr_span(expr)) | errors]}}

                {:error, :wrong_arity} ->
                  {:cont,
                   {:ok, acc_env,
                    [
                      BeamLang.Error.new(:type, "Function called with wrong number of arguments.", expr_span(expr))
                      | errors
                    ]}}

                {:error, :unknown_variable} ->
                  {:cont,
                   {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown variable in assignment.", expr_span(expr)) | errors]}}

                {:error, :unknown_type} ->
                  {:cont,
                   {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown type in assignment.", expr_span(expr)) | errors]}}

                {:error, :missing_type_annotation} ->
                  {:cont,
                   {:ok, acc_env, [BeamLang.Error.new(:type, "Struct literal requires a type annotation.", expr_span(expr)) | errors]}}

                {:error, {:struct, errs}} ->
                  {:cont, {:ok, acc_env, errs ++ errors}}

                {:error, {:match, errs}} ->
                  {:cont, {:ok, acc_env, errs ++ errors}}

                {:error, {:call, errs}} ->
                  {:cont, {:ok, acc_env, errs ++ errors}}

                {:ok, inferred} ->
                  if type_compatible?(declared, inferred) do
                    {:cont, {:ok, acc_env, errors}}
                  else
                    {:cont,
                     {:ok, acc_env,
                      [
                        BeamLang.Error.new(
                          :type,
                          "Type mismatch in assignment. Expected #{type_label(declared)}, got #{type_label(inferred)}.",
                          expr_span(expr)
                        )
                        | errors
                      ]}}
                  end
              end

            {:ok, %{name: _name, type: declared, mutable: true, field: field}} ->
              case resolve_field_type(declared, field, type_table) do
                {:error, err} ->
                  {:cont, {:ok, acc_env, [err | errors]}}

                {:ok, field_type} ->
                  case type_of_expr(expr, func_table, type_table, acc_env, field_type) do
                    {:error, :unknown_function} ->
                      {:cont,
                       {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown function in assignment.", expr_span(expr)) | errors]}}

                    {:error, :wrong_arity} ->
                      {:cont,
                       {:ok, acc_env,
                        [
                          BeamLang.Error.new(:type, "Function called with wrong number of arguments.", expr_span(expr))
                          | errors
                        ]}}

                    {:error, :unknown_variable} ->
                      {:cont,
                       {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown variable in assignment.", expr_span(expr)) | errors]}}

                    {:error, :unknown_type} ->
                      {:cont,
                       {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown type in assignment.", expr_span(expr)) | errors]}}

                    {:error, :missing_type_annotation} ->
                      {:cont,
                       {:ok, acc_env, [BeamLang.Error.new(:type, "Struct literal requires a type annotation.", expr_span(expr)) | errors]}}

                    {:error, {:struct, errs}} ->
                      {:cont, {:ok, acc_env, errs ++ errors}}

                    {:error, {:match, errs}} ->
                      {:cont, {:ok, acc_env, errs ++ errors}}

                    {:error, {:call, errs}} ->
                      {:cont, {:ok, acc_env, errs ++ errors}}

                    {:ok, inferred} ->
                      if type_compatible?(field_type, inferred) do
                        {:cont, {:ok, acc_env, errors}}
                      else
                        {:cont,
                         {:ok, acc_env,
                          [
                            BeamLang.Error.new(
                              :type,
                              "Type mismatch in assignment. Expected #{type_label(field_type)}, got #{type_label(inferred)}.",
                              expr_span(expr)
                            )
                            | errors
                          ]}}
                      end
                  end
              end
          end

        {:return, %{expr: nil}} ->
          {:cont, {:ok, acc_env, errors}}

        {:return, %{expr: expr}} ->
          case type_of_expr(expr, func_table, type_table, acc_env, nil) do
            {:error, :unknown_function} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown function in expression.", expr_span(expr)) | errors]}}

            {:error, :wrong_arity} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(:type, "Function called with wrong number of arguments.", expr_span(expr))
                  | errors
                ]}}

            {:error, :unknown_variable} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown variable in expression.", expr_span(expr)) | errors]}}

            {:error, :unknown_type} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown type in expression.", expr_span(expr)) | errors]}}

            {:error, :missing_type_annotation} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Struct literal requires a type annotation.", expr_span(expr)) | errors]}}

            {:error, :not_a_struct} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Field access requires a struct value.", expr_span(expr)) | errors]}}

            {:error, {:unknown_field, field}} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown field '#{field}'.", expr_span(expr)) | errors]}}

            {:error, {:struct, errs}} ->
              {:cont, {:ok, acc_env, errs ++ errors}}

            {:error, {:match, errs}} ->
              {:cont, {:ok, acc_env, errs ++ errors}}

            {:error, {:call, errs}} ->
              {:cont, {:ok, acc_env, errs ++ errors}}

            {:ok, _} ->
              {:cont, {:ok, acc_env, errors}}
          end

        {:expr, %{expr: expr}} ->
          case type_of_expr(expr, func_table, type_table, acc_env, nil) do
            {:error, :unknown_function} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown function in expression.", expr_span(expr)) | errors]}}

            {:error, :wrong_arity} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(:type, "Function called with wrong number of arguments.", expr_span(expr))
                  | errors
                ]}}

            {:error, :unknown_variable} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown variable in expression.", expr_span(expr)) | errors]}}

            {:error, :unknown_type} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown type in expression.", expr_span(expr)) | errors]}}

            {:error, :missing_type_annotation} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Struct literal requires a type annotation.", expr_span(expr)) | errors]}}

            {:error, :not_a_struct} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Field access requires a struct value.", expr_span(expr)) | errors]}}

            {:error, {:unknown_field, field}} ->
              {:cont,
               {:ok, acc_env, [BeamLang.Error.new(:type, "Unknown field '#{field}'.", expr_span(expr)) | errors]}}

            {:error, {:struct, errs}} ->
              {:cont, {:ok, acc_env, errs ++ errors}}

            {:error, {:match, errs}} ->
              {:cont, {:ok, acc_env, errs ++ errors}}

            {:error, {:call, errs}} ->
              {:cont, {:ok, acc_env, errs ++ errors}}

            {:ok, _} ->
              {:cont, {:ok, acc_env, errors}}
          end
      end
    end)
    |> finalize_errors()
  end

  @spec build_function_table([BeamLang.AST.func()]) :: {:ok, map()}
  defp build_function_table(functions) do
    table =
      Enum.reduce(functions, stdlib_functions(), fn {:function, %{name: name, params: params, return_type: return_type}}, acc ->
        param_types = Enum.map(params, & &1.type)
        Map.put(acc, name, {param_types, return_type})
      end)

    {:ok, table}
  end

  @spec stdlib_functions() :: map()
  defp stdlib_functions() do
    %{
      "println" => {[:String], :void},
      "print" => {[:String], :void}
    }
  end

  @spec build_type_table([BeamLang.AST.type_def()]) :: {:ok, map()}
  defp build_type_table(types) do
    table =
      Enum.reduce(types, %{}, fn {:type_def, %{name: name, fields: fields}}, acc ->
        field_map =
          Enum.reduce(fields, %{}, fn %{name: fname, type: ftype}, f_acc ->
            Map.put(f_acc, fname, ftype)
          end)

        Map.put(acc, name, field_map)
      end)

    {:ok, table}
  end

  @spec collect_function_errors([BeamLang.AST.func()], map(), map()) :: [BeamLang.Error.t()]
  defp collect_function_errors(functions, func_table, type_table) do
    functions
    |> Enum.flat_map(&validate_function(&1, func_table, type_table))
  end

  @spec collect_type_errors([BeamLang.AST.type_def()], map()) :: [BeamLang.Error.t()]
  defp collect_type_errors(_types, _type_table), do: []

  @spec collect_main_error([BeamLang.AST.func()]) :: [BeamLang.Error.t()]
  defp collect_main_error(functions) do
    case require_main_exists(functions) do
      :ok -> []
      {:error, errs} -> errs
    end
  end

  @spec validate_struct_fields([BeamLang.AST.field_assign()], map(), map(), map(), map(), binary()) ::
          [BeamLang.Error.t()]
  defp validate_struct_fields(fields, type_fields, func_table, type_table, env, type_name) do
    provided = Map.new(fields, fn %{name: name} -> {name, true} end)

    missing =
      type_fields
      |> Map.keys()
      |> Enum.reject(&Map.has_key?(provided, &1))

    missing_errors =
      Enum.map(missing, fn field ->
        BeamLang.Error.new(:type, "Missing field '#{field}' for #{type_name}.", BeamLang.Span.new("<source>", 0, 0))
      end)

    field_errors =
      Enum.flat_map(fields, fn %{name: name, expr: expr, span: span} ->
        case Map.fetch(type_fields, name) do
          :error ->
            [BeamLang.Error.new(:type, "Unknown field '#{name}' in struct literal.", span)]

          {:ok, field_type} ->
            case type_of_expr(expr, func_table, type_table, env, field_type) do
              {:ok, inferred} ->
                if type_compatible?(field_type, inferred) do
                  []
                else
                  [
                    BeamLang.Error.new(
                      :type,
                      "Field '#{name}' expected #{type_label(field_type)}, got #{type_label(inferred)}.",
                      expr_span(expr)
                    )
                  ]
                end

              {:error, :unknown_variable} ->
                [BeamLang.Error.new(:type, "Unknown variable in field '#{name}'.", expr_span(expr))]

              {:error, :unknown_function} ->
                [BeamLang.Error.new(:type, "Unknown function in field '#{name}'.", expr_span(expr))]

              {:error, _} ->
                [BeamLang.Error.new(:type, "Invalid value for field '#{name}'.", expr_span(expr))]
            end
        end
      end)

    missing_errors ++ field_errors
  end

  @spec finalize_errors({:ok, map(), [BeamLang.Error.t()]}) ::
          {:ok, map(), [BeamLang.Error.t()]}
  defp finalize_errors({:ok, env, errors}), do: {:ok, env, Enum.reverse(errors)}

  @spec block_expr_type(BeamLang.AST.block(), map(), map(), map()) ::
          {BeamLang.AST.type_name(), [BeamLang.Error.t()]}
  defp block_expr_type({:block, %{stmts: stmts}} = block, func_table, type_table, env) do
    {:ok, _env, stmt_errors} = validate_statements(block, func_table, type_table, env)

    type =
      case List.last(stmts) do
        {:return, %{expr: nil}} -> :void
        {:return, %{expr: expr}} -> type_or_unknown(expr, func_table, type_table, env)
        {:expr, %{expr: expr}} -> type_or_unknown(expr, func_table, type_table, env)
        _ -> :void
      end

    {type, stmt_errors}
  end

  @spec case_body_type(BeamLang.AST.expr(), map(), map(), map()) ::
          {BeamLang.AST.type_name(), [BeamLang.Error.t()]}
  defp case_body_type({:block_expr, %{block: block}}, func_table, type_table, env) do
    block_expr_type(block, func_table, type_table, env)
  end

  defp case_body_type(expr, func_table, type_table, env) do
    case type_of_expr(expr, func_table, type_table, env, nil) do
      {:ok, type} -> {type, []}
      {:error, reason} -> {:void, expr_error(reason, expr)}
    end
  end

  @spec type_or_unknown(BeamLang.AST.expr(), map(), map(), map()) :: BeamLang.AST.type_name()
  defp type_or_unknown(expr, func_table, type_table, env) do
    case type_of_expr(expr, func_table, type_table, env, nil) do
      {:ok, type} -> type
      {:error, _} -> :void
    end
  end

  @spec pattern_bindings(BeamLang.AST.pattern(), BeamLang.AST.type_name(), map()) ::
          {map(), [BeamLang.Error.t()]}
  defp pattern_bindings({:wildcard, %{}}, _match_type, _type_table), do: {%{}, []}

  defp pattern_bindings({:pat_identifier, %{name: name}}, match_type, _type_table) do
    {%{name => %{type: match_type, mutable: false}}, []}
  end

  defp pattern_bindings({:integer, _} = pattern, match_type, _type_table) do
    literal_pattern_bindings(pattern, match_type)
  end

  defp pattern_bindings({:float, _} = pattern, match_type, _type_table) do
    literal_pattern_bindings(pattern, match_type)
  end

  defp pattern_bindings({:string, _} = pattern, match_type, _type_table) do
    literal_pattern_bindings(pattern, match_type)
  end

  defp pattern_bindings({:bool, _} = pattern, match_type, _type_table) do
    literal_pattern_bindings(pattern, match_type)
  end

  defp pattern_bindings({:struct_pattern, %{name: name, fields: fields, span: span}}, match_type, type_table) do
    case match_type do
      {:named, ^name} ->
        case Map.fetch(type_table, name) do
          {:ok, field_map} ->
            Enum.reduce(fields, {%{}, []}, fn %{name: field_name, pattern: pattern, span: field_span},
                                             {env, errors} ->
              case Map.fetch(field_map, field_name) do
                :error ->
                  {env,
                   [BeamLang.Error.new(:type, "Unknown field '#{field_name}' in struct pattern.", field_span) | errors]}

                {:ok, field_type} ->
                  {bindings, field_errors} = pattern_bindings(pattern, field_type, type_table)
                  {Map.merge(env, bindings), errors ++ field_errors}
              end
            end)

          :error ->
            {%{}, [BeamLang.Error.new(:type, "Unknown type '#{name}' in struct pattern.", span)]}
        end

      {:named, other} ->
        {%{}, [BeamLang.Error.new(:type, "Struct pattern expects #{name}, got #{type_label({:named, other})}.", span)]}

      _ ->
        {%{}, [BeamLang.Error.new(:type, "Struct pattern requires a struct value.", span)]}
    end
  end

  @spec literal_pattern_bindings(BeamLang.AST.literal(), BeamLang.AST.type_name()) ::
          {map(), [BeamLang.Error.t()]}
  defp literal_pattern_bindings(pattern, match_type) do
    literal_type = literal_type(pattern)

    if type_compatible?(match_type, literal_type) do
      {%{}, []}
    else
      {%{},
       [
         BeamLang.Error.new(
           :type,
           "Pattern type mismatch. Expected #{type_label(match_type)}, got #{type_label(literal_type)}.",
           pattern_span(pattern)
         )
       ]}
    end
  end

  @spec literal_type(BeamLang.AST.literal()) :: BeamLang.AST.type_name()
  defp literal_type({:integer, _}), do: :integer
  defp literal_type({:float, _}), do: :float
  defp literal_type({:string, _}), do: :String
  defp literal_type({:bool, _}), do: :bool

  @spec mismatch_type(BeamLang.AST.type_name(), [{BeamLang.AST.type_name(), BeamLang.Span.t()}]) ::
          :ok | {:error, [BeamLang.Error.t()]}
  defp mismatch_type(expected, case_types) do
    errors =
      Enum.flat_map(case_types, fn {type, span} ->
        if type_compatible?(expected, type) do
          []
        else
          [
            BeamLang.Error.new(
              :type,
              "Match case type mismatch. Expected #{type_label(expected)}, got #{type_label(type)}.",
              span
            )
          ]
        end
      end)

    if errors == [], do: :ok, else: {:error, errors}
  end

  @spec expr_error(any(), BeamLang.AST.expr()) :: [BeamLang.Error.t()]
  defp expr_error(reason, expr) do
    case reason do
      :unknown_function ->
        [BeamLang.Error.new(:type, "Unknown function in expression.", expr_span(expr))]

      :unknown_variable ->
        [BeamLang.Error.new(:type, "Unknown variable in expression.", expr_span(expr))]

      :unknown_type ->
        [BeamLang.Error.new(:type, "Unknown type in expression.", expr_span(expr))]

      :missing_type_annotation ->
        [BeamLang.Error.new(:type, "Struct literal requires a type annotation.", expr_span(expr))]

      :not_a_struct ->
        [BeamLang.Error.new(:type, "Field access requires a struct value.", expr_span(expr))]

      {:unknown_field, field} ->
        [BeamLang.Error.new(:type, "Unknown field '#{field}'.", expr_span(expr))]

      {:struct, errs} ->
        errs

      {:match, errs} ->
        errs

      {:call, errs} ->
        errs

      :wrong_arity ->
        [BeamLang.Error.new(:type, "Function called with wrong number of arguments.", expr_span(expr))]

      :invalid_binary_op ->
        [BeamLang.Error.new(:type, "Invalid operands for comparison.", expr_span(expr))]

      _ ->
        [BeamLang.Error.new(:type, "Invalid expression.", expr_span(expr))]
    end
  end

  @spec pattern_span(BeamLang.AST.pattern()) :: BeamLang.Span.t()
  defp pattern_span({:integer, %{span: span}}), do: span
  defp pattern_span({:float, %{span: span}}), do: span
  defp pattern_span({:string, %{span: span}}), do: span
  defp pattern_span({:bool, %{span: span}}), do: span
  defp pattern_span({:wildcard, %{span: span}}), do: span
  defp pattern_span({:pat_identifier, %{span: span}}), do: span
  defp pattern_span({:struct_pattern, %{span: span}}), do: span

  @spec guard_errors(BeamLang.AST.expr() | nil, map(), map(), map()) :: [BeamLang.Error.t()]
  defp guard_errors(nil, _func_table, _type_table, _env), do: []

  defp guard_errors(expr, func_table, type_table, env) do
    if guard_allowed?(expr) do
      case type_of_expr(expr, func_table, type_table, env, :bool) do
        {:ok, :bool} ->
          []

        {:ok, type} ->
          [
            BeamLang.Error.new(
              :type,
              "Match guard must be bool, got #{type_label(type)}.",
              expr_span(expr)
            )
          ]

        {:error, reason} ->
          expr_error(reason, expr)
      end
    else
      [BeamLang.Error.new(:type, "Match guard only supports comparison operators.", expr_span(expr))]
    end
  end

  @spec guard_allowed?(BeamLang.AST.expr()) :: boolean()
  defp guard_allowed?({:binary, %{left: left, right: right}}) do
    guard_operand?(left) and guard_operand?(right)
  end

  defp guard_allowed?(_expr), do: false

  @spec guard_operand?(BeamLang.AST.expr()) :: boolean()
  defp guard_operand?({:integer, _}), do: true
  defp guard_operand?({:float, _}), do: true
  defp guard_operand?({:string, _}), do: true
  defp guard_operand?({:bool, _}), do: true
  defp guard_operand?({:identifier, _}), do: true
  defp guard_operand?(_), do: false

  @spec condition_errors(BeamLang.AST.expr(), map(), map(), map()) :: [BeamLang.Error.t()]
  defp condition_errors(cond, func_table, type_table, env) do
    case type_of_expr(cond, func_table, type_table, env, :bool) do
      {:ok, :bool} ->
        []

      {:ok, type} ->
        [
          BeamLang.Error.new(
            :type,
            "Condition must be bool, got #{type_label(type)}.",
            expr_span(cond)
          )
        ]

      {:error, reason} ->
        expr_error(reason, cond)
    end
  end

  @spec else_errors(BeamLang.AST.if_else_branch() | nil, map(), map(), map(), non_neg_integer(), [BeamLang.Error.t()]) ::
          {[BeamLang.Error.t()], map()}
  defp else_errors(nil, _func_table, _type_table, _env, _loop_depth, errors), do: {errors, %{}}

  defp else_errors({:else_block, %{block: block}}, func_table, type_table, env, loop_depth, errors) do
    {:ok, _env_else, else_errors} = validate_statements(block, func_table, type_table, env, loop_depth)
    {else_errors ++ errors, %{}}
  end

  defp else_errors({:else_if, %{if: if_stmt}}, func_table, type_table, env, loop_depth, errors) do
    {:ok, _env, if_errors} = validate_statements({:block, %{stmts: [if_stmt], span: BeamLang.Span.new("<source>", 0, 0)}},
      func_table, type_table, env, loop_depth)
    {if_errors ++ errors, %{}}
  end

  @spec else_branch_type(BeamLang.AST.if_else_branch(), map(), map(), map()) ::
          {BeamLang.AST.type_name(), [BeamLang.Error.t()]}
  defp else_branch_type({:else_block, %{block: block}}, func_table, type_table, env) do
    block_expr_type(block, func_table, type_table, env)
  end

  defp else_branch_type({:else_if, %{if: if_stmt}}, func_table, type_table, env) do
    case if_stmt do
      {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}} ->
        if else_branch == nil do
          {then_type, then_errors} = block_expr_type(then_block, func_table, type_table, env)
          {then_type,
           [
             BeamLang.Error.new(
               :type,
               "If expression requires else branch.",
               expr_span(cond)
             )
           ] ++ then_errors}
        else
          errors = condition_errors(cond, func_table, type_table, env)
          {then_type, then_errors} = block_expr_type(then_block, func_table, type_table, env)
          {else_type, else_errors} = else_branch_type(else_branch, func_table, type_table, env)
          errors = errors ++ then_errors ++ else_errors

          if errors != [] do
            {:void, errors}
          else
            if type_compatible?(then_type, else_type) do
              {then_type, []}
            else
              {then_type,
               [
                 BeamLang.Error.new(
                   :type,
                   "If expression type mismatch. Expected #{type_label(then_type)}, got #{type_label(else_type)}.",
                   expr_span(cond)
                 )
               ]}
            end
          end
        end
    end
  end

  @spec iterator_errors(BeamLang.AST.expr(), map(), map(), map(), BeamLang.Span.t()) :: [BeamLang.Error.t()]
  defp iterator_errors(collection, func_table, type_table, env, span) do
    case type_of_expr(collection, func_table, type_table, env, :String) do
      {:ok, :String} -> []
      {:ok, type} ->
        [BeamLang.Error.new(:type, "For loop requires String iterable, got #{type_label(type)}.", span)]

      {:error, reason} ->
        expr_error(reason, collection)
    end
  end

  @spec iterator_item_type(BeamLang.AST.expr(), map(), map(), map()) :: BeamLang.AST.type_name()
  defp iterator_item_type(_collection, _func_table, _type_table, _env), do: :i32

  @spec expr_span(BeamLang.AST.expr()) :: BeamLang.Span.t()
  defp expr_span({:integer, %{span: span}}), do: span
  defp expr_span({:float, %{span: span}}), do: span
  defp expr_span({:string, %{span: span}}), do: span
  defp expr_span({:bool, %{span: span}}), do: span
  defp expr_span({:call, %{span: span}}), do: span
  defp expr_span({:identifier, %{span: span}}), do: span
  defp expr_span({:struct, %{span: span}}), do: span
  defp expr_span({:field, %{span: span}}), do: span
  defp expr_span({:block_expr, %{span: span}}), do: span
  defp expr_span({:match, %{span: span}}), do: span
  defp expr_span({:binary, %{span: span}}), do: span
  defp expr_span({:if_expr, %{span: span}}), do: span

  @spec type_compatible?(BeamLang.AST.type_name(), BeamLang.AST.type_name()) :: boolean()
  defp type_compatible?(expected, inferred) when expected == inferred, do: true
  defp type_compatible?(expected, :integer) when expected in [:i32, :i64], do: true
  defp type_compatible?(expected, :float) when expected in [:f32, :f64], do: true
  defp type_compatible?(_expected, _inferred), do: false

  @spec comparable_types?(BeamLang.AST.type_name(), BeamLang.AST.type_name()) :: boolean()
  defp comparable_types?(left, right) when left == right, do: true
  defp comparable_types?(left, :integer) when left in [:i32, :i64], do: true
  defp comparable_types?(:integer, right) when right in [:i32, :i64], do: true
  defp comparable_types?(left, :float) when left in [:f32, :f64], do: true
  defp comparable_types?(:float, right) when right in [:f32, :f64], do: true
  defp comparable_types?(left, right) when left in [:i32, :i64] and right in [:i32, :i64], do: true
  defp comparable_types?(left, right) when left in [:f32, :f64] and right in [:f32, :f64], do: true
  defp comparable_types?(_left, _right), do: false

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

  @spec assignment_target(BeamLang.AST.expr(), map()) ::
          {:ok, %{name: binary(), type: BeamLang.AST.type_name(), mutable: boolean(), field: binary() | nil}}
          | {:error, BeamLang.Error.t()}
  defp assignment_target({:identifier, %{name: name, span: span}}, env) do
    case Map.fetch(env, name) do
      {:ok, %{type: type, mutable: mutable}} -> {:ok, %{name: name, type: type, mutable: mutable, field: nil}}
      :error -> {:error, BeamLang.Error.new(:type, "Unknown variable in assignment.", span)}
    end
  end

  defp assignment_target({:field, %{target: target, name: field, span: span}}, env) do
    case target do
      {:identifier, %{name: name}} ->
        case Map.fetch(env, name) do
          {:ok, %{type: type, mutable: mutable}} -> {:ok, %{name: name, type: type, mutable: mutable, field: field}}
          :error -> {:error, BeamLang.Error.new(:type, "Unknown variable in assignment.", span)}
        end

      _ ->
        {:error, BeamLang.Error.new(:type, "Assignment target must be a variable or struct field.", span)}
    end
  end

  defp assignment_target(_other, _env) do
    {:error, BeamLang.Error.new(:type, "Invalid assignment target.", BeamLang.Span.new("<source>", 0, 0))}
  end

  @spec resolve_field_type(BeamLang.AST.type_name(), binary(), map()) ::
          {:ok, BeamLang.AST.type_name()} | {:error, BeamLang.Error.t()}
  defp resolve_field_type({:named, type_name}, field, type_table) do
    case Map.fetch(type_table, type_name) do
      {:ok, field_map} ->
        case Map.fetch(field_map, field) do
          {:ok, field_type} -> {:ok, field_type}
          :error -> {:error, BeamLang.Error.new(:type, "Unknown field '#{field}'.", BeamLang.Span.new("<source>", 0, 0))}
        end

      :error ->
        {:error, BeamLang.Error.new(:type, "Unknown type '#{type_name}'.", BeamLang.Span.new("<source>", 0, 0))}
    end
  end

  defp resolve_field_type(_other, _field, _type_table) do
    {:error, BeamLang.Error.new(:type, "Field access requires a struct value.", BeamLang.Span.new("<source>", 0, 0))}
  end

  @spec block_span([BeamLang.AST.stmt()]) :: BeamLang.Span.t()
  defp block_span([]), do: BeamLang.Span.new("<source>", 0, 0)

  defp block_span([first | _] = stmts) do
    first_span = stmt_span(first)
    last_span = stmt_span(List.last(stmts))
    BeamLang.Span.merge(first_span, last_span)
  end

  @spec block_span(BeamLang.AST.block()) :: BeamLang.Span.t()
  defp block_span({:block, %{stmts: stmts}}), do: block_span(stmts)

  @spec guard_has_return?(BeamLang.AST.block()) :: boolean()
  defp guard_has_return?({:block, %{stmts: stmts}}) do
    case List.last(stmts) do
      {:return, _} -> true
      _ -> false
    end
  end

  @spec block_returns?(BeamLang.AST.block()) :: boolean()
  defp block_returns?({:block, %{stmts: stmts}}) do
    case List.last(stmts) do
      nil -> false
      stmt -> stmt_returns?(stmt)
    end
  end

  @spec stmt_returns?(BeamLang.AST.stmt()) :: boolean()
  defp stmt_returns?({:return, _}), do: true

  defp stmt_returns?({:if_stmt, %{then_block: then_block, else_branch: else_branch}}) do
    block_returns?(then_block) and else_branch_returns?(else_branch)
  end

  defp stmt_returns?(_stmt), do: false

  @spec else_branch_returns?(BeamLang.AST.if_else_branch() | nil) :: boolean()
  defp else_branch_returns?(nil), do: false
  defp else_branch_returns?({:else_block, %{block: block}}), do: block_returns?(block)
  defp else_branch_returns?({:else_if, %{if: if_stmt}}), do: stmt_returns?(if_stmt)

  @spec select_match_type([{BeamLang.AST.type_name(), BeamLang.Span.t()}]) :: BeamLang.AST.type_name()
  defp select_match_type(case_types) do
    preferred =
      case_types
      |> Enum.map(&elem(&1, 0))
      |> Enum.reject(&(&1 in [:integer, :float, :void]))

    case preferred do
      [first | _] -> first
      _ -> elem(List.first(case_types), 0)
    end
  end

  @spec type_label(BeamLang.AST.type_name()) :: binary()
  defp type_label({:named, name}) when is_binary(name), do: name
  defp type_label(:integer), do: "i32"
  defp type_label(:float), do: "f32"
  defp type_label(type) when is_atom(type), do: Atom.to_string(type)
end
