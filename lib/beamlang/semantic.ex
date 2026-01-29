defmodule BeamLang.Semantic do
  @moduledoc """
  Performs basic semantic checks for the MVP.
  """

  @spec validate(BeamLang.AST.t(), keyword()) :: {:ok, BeamLang.AST.t()} | {:error, [BeamLang.Error.t()]}
  def validate({:program, %{types: types, functions: functions}} = ast, opts \\ []) when is_list(functions) do
    require_main = Keyword.get(opts, :require_main, true)
    {:ok, func_table} = build_function_table(functions)
    {:ok, type_table} = build_type_table(types)

    errors =
      (if require_main, do: collect_main_error(functions), else: []) ++
        collect_type_errors(types, type_table) ++
        collect_function_errors(functions, func_table, type_table)

    case errors do
      [] -> {:ok, annotate_program(ast, func_table, type_table)}
      _ -> {:error, errors}
    end
  end

  @spec require_main_exists([BeamLang.AST.func()]) :: :ok | {:error, [BeamLang.Error.t()]}
  defp require_main_exists(functions) do
    case Enum.find(functions, fn {:function, %{name: name, body: body}} -> name == "main" and body != nil end) do
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

  @spec require_main_number(binary(), BeamLang.AST.type_name()) :: :ok | {:error, [BeamLang.Error.t()]}
  defp require_main_number("main", :number), do: :ok

  defp require_main_number("main", other),
    do:
      {:error,
       [
         BeamLang.Error.new(
           :type,
           "main must return number, got #{type_label(other)}.",
           BeamLang.Span.new("<source>", 0, 0)
         )
       ]}

  defp require_main_number(_name, _type), do: :ok

  @spec type_of_expr(BeamLang.AST.expr(), map(), map(), map(), BeamLang.AST.type_name() | nil) ::
          {:ok, BeamLang.AST.type_name()}
          | {:error,
             :unknown_function
             | :wrong_arity
             | :unknown_variable
             | :unknown_type
             | :missing_type_annotation
             | :missing_optional_context
             | :missing_result_context
             | {:struct, [BeamLang.Error.t()]}
             | {:match, [BeamLang.Error.t()]}
             | {:call, [BeamLang.Error.t()]}
             | :invalid_binary_op
             | {:unknown_field, binary()}
             | :not_a_struct}
  defp type_of_expr({:integer, %{value: _value}}, _func_table, _type_table, _env, _expected), do: {:ok, :number}
  defp type_of_expr({:float, %{value: _value}}, _func_table, _type_table, _env, _expected), do: {:ok, :number}
  defp type_of_expr({:string, %{value: _value}}, _func_table, _type_table, _env, _expected), do: {:ok, :String}
  defp type_of_expr({:char, %{value: _value}}, _func_table, _type_table, _env, _expected), do: {:ok, :char}
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
    case struct_type_info(expected) do
      {:ok, type_name, args} ->
        case Map.fetch(type_table, type_name) do
          {:ok, %{params: params, fields: field_map}} ->
            case type_arg_map(params, args) do
              {:ok, param_map} ->
                type_fields = substitute_field_types(field_map, param_map)
                errors = validate_struct_fields(fields, type_fields, func_table, type_table, env, type_name)
                if errors == [], do: {:ok, expected}, else: {:error, {:struct, errors}}

              :error ->
                {:error, :unknown_type}
            end

          :error ->
            {:error, :unknown_type}
        end

      :missing ->
        {:error, :missing_type_annotation}

      :error ->
        {:error, :unknown_type}
    end
  end

  defp type_of_expr({:opt_some, %{expr: expr}}, func_table, type_table, env, expected) do
    case expected do
      {:optional, inner} ->
        case type_of_expr(expr, func_table, type_table, env, inner) do
          {:ok, inferred} ->
            if type_compatible?(inner, inferred) do
              {:ok, {:optional, inner}}
            else
              {:error,
               {:match,
                [
                  BeamLang.Error.new(
                    :type,
                    "Optional value type mismatch. Expected #{type_label(inner)}, got #{type_label(inferred)}.",
                    expr_span(expr)
                  )
                ]}}
            end

          {:error, reason} ->
            {:error, reason}
        end

      _ ->
        {:error, :missing_optional_context}
    end
  end

  defp type_of_expr({:opt_none, %{span: _span}}, _func_table, _type_table, _env, expected) do
    case expected do
      {:optional, inner} -> {:ok, {:optional, inner}}
      _ -> {:error, :missing_optional_context}
    end
  end

  defp type_of_expr({:res_ok, %{expr: expr}}, func_table, type_table, env, expected) do
    case expected do
      {:result, ok_type, err_type} ->
        case type_of_expr(expr, func_table, type_table, env, ok_type) do
          {:ok, inferred} ->
            if type_compatible?(ok_type, inferred) do
              {:ok, {:result, ok_type, err_type}}
            else
              {:error,
               {:match,
                [
                  BeamLang.Error.new(
                    :type,
                    "Result ok type mismatch. Expected #{type_label(ok_type)}, got #{type_label(inferred)}.",
                    expr_span(expr)
                  )
                ]}}
            end

          {:error, reason} ->
            {:error, reason}
        end

      _ ->
        {:error, :missing_result_context}
    end
  end

  defp type_of_expr({:res_err, %{expr: expr}}, func_table, type_table, env, expected) do
    case expected do
      {:result, ok_type, err_type} ->
        case type_of_expr(expr, func_table, type_table, env, err_type) do
          {:ok, inferred} ->
            if type_compatible?(err_type, inferred) do
              {:ok, {:result, ok_type, err_type}}
            else
              {:error,
               {:match,
                [
                  BeamLang.Error.new(
                    :type,
                    "Result err type mismatch. Expected #{type_label(err_type)}, got #{type_label(inferred)}.",
                    expr_span(expr)
                  )
                ]}}
            end

          {:error, reason} ->
            {:error, reason}
        end

      _ ->
        {:error, :missing_result_context}
    end
  end

  defp type_of_expr({:field, %{target: target, name: name}}, func_table, type_table, env, _expected) do
    case type_of_expr(target, func_table, type_table, env, nil) do
      {:ok, type} ->
        case struct_type_info(type) do
          {:ok, type_name, args} ->
            case Map.fetch(type_table, type_name) do
              {:ok, %{params: params, fields: field_map}} ->
                case type_arg_map(params, args) do
                  {:ok, param_map} ->
                    type_fields = substitute_field_types(field_map, param_map)

                    case Map.fetch(type_fields, name) do
                      {:ok, field_type} -> {:ok, field_type}
                      :error -> {:error, {:unknown_field, name}}
                    end

                  :error ->
                    {:error, :unknown_type}
                end

              :error ->
                {:error, :unknown_type}
            end

          :error ->
            {:error, :not_a_struct}
        end

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

  defp type_of_expr({:binary, %{op: op, left: left, right: right}}, func_table, type_table, env, _expected) do
    with {:ok, left_type} <- type_of_expr(left, func_table, type_table, env, nil),
         {:ok, right_type} <- type_of_expr(right, func_table, type_table, env, nil) do
      cond do
        op in [:eq, :neq, :lt, :gt, :lte, :gte] ->
          if comparable_types?(left_type, right_type) do
            {:ok, :bool}
          else
            {:error, :invalid_binary_op}
          end

        op in [:add, :sub, :mul, :div, :mod] ->
          arithmetic_type(op, left_type, right_type)

        true ->
          {:error, :invalid_binary_op}
      end
    end
  end

  @spec validate_function(BeamLang.AST.func(), map(), map()) :: [BeamLang.Error.t()]
  defp validate_function({:function, %{name: name, params: params, return_type: return_type, body: body}}, func_table, type_table) do
    errors = []

    param_env =
      Enum.reduce(params, %{}, fn %{name: param_name, type: param_type}, acc ->
        Map.put(acc, param_name, %{type: normalize_type(param_type), mutable: false})
      end)

    return_type = normalize_type(return_type)

    {env, stmt_errors} =
      case body do
        nil -> {param_env, []}
        _ ->
          case validate_statements(body, func_table, type_table, param_env, return_type) do
            {:ok, env, errs} -> {env, errs}
          end
      end

    errors = errors ++ stmt_errors

    errors =
      case body do
        nil ->
          errors

        _ ->
          case typecheck_return(return_type, body, func_table, type_table, env) do
            :ok -> errors
            {:error, errs} -> errors ++ errs
          end
      end

    errors =
      case require_main_number(name, return_type) do
        :ok -> errors
        {:error, errs} -> errors ++ errs
      end

    errors
  end

  @spec validate_statements(BeamLang.AST.block(), map(), map(), map()) ::
          {:ok, map(), [BeamLang.Error.t()]}
  defp validate_statements(block, func_table, type_table, env) do
    validate_statements(block, func_table, type_table, env, 0, nil)
  end

  @spec validate_statements(BeamLang.AST.block(), map(), map(), map(), BeamLang.AST.type_name() | nil) ::
          {:ok, map(), [BeamLang.Error.t()]}
  defp validate_statements(block, func_table, type_table, env, return_type) do
    validate_statements(block, func_table, type_table, env, 0, return_type)
  end

  @spec validate_statements(BeamLang.AST.block(), map(), map(), map(), non_neg_integer(), BeamLang.AST.type_name() | nil) ::
          {:ok, map(), [BeamLang.Error.t()]}
  defp validate_statements({:block, %{stmts: stmts}}, func_table, type_table, env, loop_depth, return_type) do
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
          {:ok, _env_then, then_errors} =
            validate_statements(then_block, func_table, type_table, acc_env, loop_depth, return_type)
          errors = then_errors ++ errors
          {errors, _} =
            else_errors(else_branch, func_table, type_table, acc_env, loop_depth, return_type, errors)
          {:cont, {:ok, acc_env, errors}}

        {:while, %{cond: cond, body: body}} ->
          errors = errors ++ condition_errors(cond, func_table, type_table, acc_env)
          {:ok, _env_body, body_errors} =
            validate_statements(body, func_table, type_table, acc_env, loop_depth + 1, return_type)
          {:cont, {:ok, acc_env, body_errors ++ errors}}

        {:loop, %{body: body}} ->
          {:ok, _env_body, body_errors} =
            validate_statements(body, func_table, type_table, acc_env, loop_depth + 1, return_type)
          {:cont, {:ok, acc_env, body_errors ++ errors}}

        {:for, %{name: name, collection: collection, body: body, span: span}} ->
          errors = errors ++ iterator_errors(collection, func_table, type_table, acc_env, span)
          item_type = iterator_item_type(collection, func_table, type_table, acc_env)
          loop_env = Map.put(acc_env, name, %{type: item_type, mutable: false})
          {:ok, _env_body, body_errors} =
            validate_statements(body, func_table, type_table, loop_env, loop_depth + 1, return_type)
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

          {:ok, _env, else_errors} =
            validate_statements(else_block, func_table, type_table, acc_env, loop_depth, return_type)
          errors = else_errors ++ errors

          errors =
            if guard_has_return?(else_block) do
              errors
            else
              [BeamLang.Error.new(:type, "Guard else block must end with return.", block_span(else_block)) | errors]
            end

          {:cont, {:ok, acc_env, errors}}

        {:let, %{name: name, expr: expr, mutable: mutable, type: declared_type}} ->
          declared_type = normalize_type(declared_type)

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

        {:return, %{expr: nil, span: span}} ->
          if return_type == :void or return_type == nil do
            {:cont, {:ok, acc_env, errors}}
          else
            {:cont,
             {:ok, acc_env, [BeamLang.Error.new(:type, "Non-void return requires a value.", span) | errors]}}
          end

        {:return, %{expr: expr}} ->
          case type_of_expr(expr, func_table, type_table, acc_env, return_type) do
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

            {:ok, inferred} ->
              if return_type == nil or return_type == :void or type_compatible?(return_type, inferred) do
                {:cont, {:ok, acc_env, errors}}
              else
                {:cont,
                 {:ok, acc_env,
                  [
                    BeamLang.Error.new(
                      :type,
                      "Return type mismatch. Expected #{type_label(return_type)}, got #{type_label(inferred)}.",
                      expr_span(expr)
                    )
                    | errors
                  ]}}
              end
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
      Enum.reduce(functions, %{}, fn {:function, %{name: name, params: params, return_type: return_type}}, acc ->
        param_types = Enum.map(params, &normalize_type(&1.type))
        Map.put(acc, name, {param_types, normalize_type(return_type)})
      end)

    {:ok, table}
  end

  @spec build_type_table([BeamLang.AST.type_def()]) :: {:ok, map()}
  defp build_type_table(types) do
    table =
      types
      |> Enum.reduce(%{}, fn {:type_def, %{name: name, params: params, fields: fields}}, acc ->
        field_map =
          Enum.reduce(fields, %{}, fn %{name: fname, type: ftype}, f_acc ->
            Map.put(f_acc, fname, normalize_type(ftype))
          end)

        Map.put(acc, name, %{params: params, fields: field_map})
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

  defp pattern_bindings({:opt_some_pat, %{name: name}}, match_type, _type_table) do
    case match_type do
      {:optional, inner} ->
        if name == "_" do
          {%{}, []}
        else
          {%{name => %{type: inner, mutable: false}}, []}
        end

      _ ->
        {%{}, [BeamLang.Error.new(:type, "Optional pattern requires an optional value.", BeamLang.Span.new("<source>", 0, 0))]}
    end
  end

  defp pattern_bindings({:opt_none_pat, %{span: span}}, match_type, _type_table) do
    case match_type do
      {:optional, _inner} -> {%{}, []}
      _ -> {%{}, [BeamLang.Error.new(:type, "Optional pattern requires an optional value.", span)]}
    end
  end

  defp pattern_bindings({:res_ok_pat, %{name: name, span: span}}, match_type, _type_table) do
    case match_type do
      {:result, ok_type, _err_type} ->
        if name == "_" do
          {%{}, []}
        else
          {%{name => %{type: ok_type, mutable: false}}, []}
        end

      _ ->
        {%{}, [BeamLang.Error.new(:type, "Result ok pattern requires a result value.", span)]}
    end
  end

  defp pattern_bindings({:res_err_pat, %{name: name, span: span}}, match_type, _type_table) do
    case match_type do
      {:result, _ok_type, err_type} ->
        if name == "_" do
          {%{}, []}
        else
          {%{name => %{type: err_type, mutable: false}}, []}
        end

      _ ->
        {%{}, [BeamLang.Error.new(:type, "Result err pattern requires a result value.", span)]}
    end
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
    case struct_type_info(match_type) do
      {:ok, ^name, args} ->
        case Map.fetch(type_table, name) do
          {:ok, %{params: params, fields: field_map}} ->
            case type_arg_map(params, args) do
              {:ok, param_map} ->
                type_fields = substitute_field_types(field_map, param_map)

                Enum.reduce(fields, {%{}, []}, fn %{name: field_name, pattern: pattern, span: field_span},
                                                 {env, errors} ->
                  case Map.fetch(type_fields, field_name) do
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

          :error ->
            {%{}, [BeamLang.Error.new(:type, "Unknown type '#{name}' in struct pattern.", span)]}
        end

      {:ok, other, _args} ->
        {%{}, [BeamLang.Error.new(:type, "Struct pattern expects #{name}, got #{type_label({:named, other})}.", span)]}

      :error ->
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
  defp literal_type({:integer, _}), do: :number
  defp literal_type({:float, _}), do: :number
  defp literal_type({:string, _}), do: :String
  defp literal_type({:char, _}), do: :char
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

      :missing_optional_context ->
        [BeamLang.Error.new(:type, "Optional literal requires an optional type context.", expr_span(expr))]

      :missing_result_context ->
        [BeamLang.Error.new(:type, "Result literal requires a result type context.", expr_span(expr))]

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
  defp pattern_span({:char, %{span: span}}), do: span
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
  defp guard_operand?({:char, _}), do: true
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

  @spec else_errors(BeamLang.AST.if_else_branch() | nil, map(), map(), map(), non_neg_integer(), BeamLang.AST.type_name() | nil, [BeamLang.Error.t()]) ::
          {[BeamLang.Error.t()], map()}
  defp else_errors(nil, _func_table, _type_table, _env, _loop_depth, _return_type, errors), do: {errors, %{}}

  defp else_errors({:else_block, %{block: block}}, func_table, type_table, env, loop_depth, return_type, errors) do
    {:ok, _env_else, else_errors} = validate_statements(block, func_table, type_table, env, loop_depth, return_type)
    {else_errors ++ errors, %{}}
  end

  defp else_errors({:else_if, %{if: if_stmt}}, func_table, type_table, env, loop_depth, return_type, errors) do
    {:ok, _env, if_errors} =
      validate_statements({:block, %{stmts: [if_stmt], span: BeamLang.Span.new("<source>", 0, 0)}},
        func_table, type_table, env, loop_depth, return_type)
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
    case type_of_expr(collection, func_table, type_table, env, nil) do
      {:ok, type} ->
        case iterator_item_type_from_type(type) do
          {:ok, _item_type} -> []
          :error -> [BeamLang.Error.new(:type, "For loop requires Iterator<T> or String, got #{type_label(type)}.", span)]
        end

      {:error, reason} ->
        expr_error(reason, collection)
    end
  end

  @spec iterator_item_type(BeamLang.AST.expr(), map(), map(), map()) :: BeamLang.AST.type_name()
  defp iterator_item_type(collection, func_table, type_table, env) do
    case type_of_expr(collection, func_table, type_table, env, nil) do
      {:ok, type} ->
        case iterator_item_type_from_type(type) do
          {:ok, item_type} -> item_type
          :error -> :number
        end

      {:error, _} ->
        :number
    end
  end

  defp iterator_item_type_from_type(type) do
    case normalize_type(type) do
      :String -> {:ok, :char}
      {:generic, {:named, "Iterator"}, [item_type]} -> {:ok, item_type}
      _ -> :error
    end
  end

  @spec expr_span(BeamLang.AST.expr()) :: BeamLang.Span.t()
  defp expr_span({:integer, %{span: span}}), do: span
  defp expr_span({:float, %{span: span}}), do: span
  defp expr_span({:string, %{span: span}}), do: span
  defp expr_span({:char, %{span: span}}), do: span
  defp expr_span({:bool, %{span: span}}), do: span
  defp expr_span({:call, %{span: span}}), do: span
  defp expr_span({:identifier, %{span: span}}), do: span
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

  @spec type_compatible?(BeamLang.AST.type_name(), BeamLang.AST.type_name()) :: boolean()
  defp type_compatible?(expected, inferred) do
    expected = normalize_type(expected)
    inferred = normalize_type(inferred)
    do_type_compatible?(expected, inferred)
  end

  defp do_type_compatible?(:any, _inferred), do: true
  defp do_type_compatible?(expected, inferred) when expected == inferred, do: true
  defp do_type_compatible?({:optional, expected_inner}, {:optional, inferred_inner}),
    do: type_compatible?(expected_inner, inferred_inner)
  defp do_type_compatible?({:result, expected_ok, expected_err}, {:result, inferred_ok, inferred_err}),
    do: type_compatible?(expected_ok, inferred_ok) and type_compatible?(expected_err, inferred_err)
  defp do_type_compatible?(_expected, _inferred), do: false

  @spec comparable_types?(BeamLang.AST.type_name(), BeamLang.AST.type_name()) :: boolean()
  defp comparable_types?(left, right) do
    left = normalize_type(left)
    right = normalize_type(right)
    do_comparable_types?(left, right)
  end

  defp do_comparable_types?(left, right) when left == right, do: true
  defp do_comparable_types?(_left, _right), do: false

  defp arithmetic_type(op, left, right) do
    if left == :number and right == :number and op in [:add, :sub, :mul, :div, :mod] do
      {:ok, :number}
    else
      {:error, :invalid_binary_op}
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
  defp resolve_field_type(type, field, type_table) do
    case struct_type_info(type) do
      {:ok, type_name, args} ->
        case Map.fetch(type_table, type_name) do
          {:ok, %{params: params, fields: field_map}} ->
            case type_arg_map(params, args) do
              {:ok, param_map} ->
                type_fields = substitute_field_types(field_map, param_map)

                case Map.fetch(type_fields, field) do
                  {:ok, field_type} -> {:ok, field_type}
                  :error -> {:error, BeamLang.Error.new(:type, "Unknown field '#{field}'.", BeamLang.Span.new("<source>", 0, 0))}
                end

              :error ->
                {:error, BeamLang.Error.new(:type, "Unknown type '#{type_name}'.", BeamLang.Span.new("<source>", 0, 0))}
            end

          :error ->
            {:error, BeamLang.Error.new(:type, "Unknown type '#{type_name}'.", BeamLang.Span.new("<source>", 0, 0))}
        end

      :error ->
        {:error, BeamLang.Error.new(:type, "Field access requires a struct value.", BeamLang.Span.new("<source>", 0, 0))}
    end
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

  defp stmt_returns?({:expr, %{expr: {:match, %{cases: cases}}}}) do
    Enum.all?(cases, &match_case_returns?/1)
  end

  defp stmt_returns?(_stmt), do: false

  defp match_case_returns?(%{body: {:block_expr, %{block: block}}}) do
    block_returns?(block)
  end

  defp match_case_returns?(_case), do: false

  @spec else_branch_returns?(BeamLang.AST.if_else_branch() | nil) :: boolean()
  defp else_branch_returns?(nil), do: false
  defp else_branch_returns?({:else_block, %{block: block}}), do: block_returns?(block)
  defp else_branch_returns?({:else_if, %{if: if_stmt}}), do: stmt_returns?(if_stmt)

  @spec select_match_type([{BeamLang.AST.type_name(), BeamLang.Span.t()}]) :: BeamLang.AST.type_name()
  defp select_match_type(case_types) do
    preferred =
      case_types
      |> Enum.map(&elem(&1, 0))
      |> Enum.reject(&(&1 in [:void]))

    case preferred do
      [first | _] -> first
      _ -> elem(List.first(case_types), 0)
    end
  end

  defp struct_type_info(nil), do: :missing
  defp struct_type_info({:named, name}) when is_binary(name), do: {:ok, name, []}
  defp struct_type_info({:generic, {:named, name}, args}), do: {:ok, name, args}
  defp struct_type_info(_type), do: :error

  defp annotate_program({:program, %{module: module, imports: imports, types: types, functions: functions, span: span}}, func_table, type_table) do
    functions = Enum.map(functions, &annotate_function(&1, func_table, type_table))
    {:program, %{module: module, imports: imports, types: types, functions: functions, span: span}}
  end

  defp annotate_function({:function, %{body: nil}} = func, _func_table, _type_table), do: func

  defp annotate_function({:function, %{params: params, return_type: return_type, body: body} = info}, func_table, type_table) do
    env =
      Enum.reduce(params, %{}, fn %{name: name, type: type}, acc ->
        Map.put(acc, name, %{type: normalize_type(type), mutable: false})
      end)

    {body, _env} = annotate_block(body, env, func_table, type_table, normalize_type(return_type))
    {:function, %{info | body: body}}
  end

  defp annotate_block({:block, %{stmts: stmts, span: span}}, env, func_table, type_table, return_type) do
    {stmts, env} =
      Enum.map_reduce(stmts, env, fn stmt, acc_env ->
        annotate_stmt(stmt, acc_env, func_table, type_table, return_type)
      end)

    {{:block, %{stmts: stmts, span: span}}, env}
  end

  defp annotate_stmt({:let, %{name: name, expr: expr, mutable: mutable, type: declared_type} = info}, env, func_table, type_table, _return_type) do
    declared_type = normalize_type(declared_type)
    {expr, _inferred} = annotate_expr(expr, declared_type, env, func_table, type_table)
    inferred =
      case declared_type do
        nil ->
          case type_of_expr(expr, func_table, type_table, env, nil) do
            {:ok, type} -> type
            _ -> :any
          end

        _ ->
          declared_type
      end

    env = Map.put(env, name, %{type: inferred, mutable: mutable})
    {{:let, %{info | expr: expr}}, env}
  end

  defp annotate_stmt({:assign, %{target: target, expr: expr} = info}, env, func_table, type_table, _return_type) do
    expected =
      case assignment_target(target, env) do
        {:ok, %{type: type}} -> type
        _ -> nil
      end

    {expr, _inferred} = annotate_expr(expr, expected, env, func_table, type_table)
    {{:assign, %{info | expr: expr}}, env}
  end

  defp annotate_stmt({:return, %{expr: nil}} = stmt, env, _func_table, _type_table, _return_type), do: {stmt, env}

  defp annotate_stmt({:return, %{expr: expr} = info}, env, func_table, type_table, return_type) do
    {expr, _inferred} = annotate_expr(expr, return_type, env, func_table, type_table)
    {{:return, %{info | expr: expr}}, env}
  end

  defp annotate_stmt({:expr, %{expr: expr} = info}, env, func_table, type_table, _return_type) do
    {expr, _inferred} = annotate_expr(expr, nil, env, func_table, type_table)
    {{:expr, %{info | expr: expr}}, env}
  end

  defp annotate_stmt({:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch} = info}, env, func_table, type_table, return_type) do
    {cond, _} = annotate_expr(cond, :bool, env, func_table, type_table)
    {then_block, _} = annotate_block(then_block, env, func_table, type_table, return_type)
    else_branch = annotate_else_branch(else_branch, env, func_table, type_table, return_type)
    {{:if_stmt, %{info | cond: cond, then_block: then_block, else_branch: else_branch}}, env}
  end

  defp annotate_stmt({:while, %{cond: cond, body: body} = info}, env, func_table, type_table, return_type) do
    {cond, _} = annotate_expr(cond, :bool, env, func_table, type_table)
    {body, _} = annotate_block(body, env, func_table, type_table, return_type)
    {{:while, %{info | cond: cond, body: body}}, env}
  end

  defp annotate_stmt({:loop, %{body: body} = info}, env, func_table, type_table, return_type) do
    {body, _} = annotate_block(body, env, func_table, type_table, return_type)
    {{:loop, %{info | body: body}}, env}
  end

  defp annotate_stmt({:for, %{name: name, collection: collection, body: body} = info}, env, func_table, type_table, return_type) do
    {collection, _} = annotate_expr(collection, nil, env, func_table, type_table)

    collection_type =
      case type_of_expr(collection, func_table, type_table, env, nil) do
        {:ok, type} -> normalize_type(type)
        _ -> :any
      end

    item_type =
      case iterator_item_type_from_type(collection_type) do
        {:ok, t} -> t
        :error -> :any
      end

    loop_env = Map.put(env, name, %{type: item_type, mutable: false})
    {body, _} = annotate_block(body, loop_env, func_table, type_table, return_type)
    updated = Map.merge(info, %{collection: collection, body: body, collection_type: collection_type, item_type: item_type})
    {{:for, updated}, env}
  end

  defp annotate_stmt({:guard, %{cond: cond, else_block: else_block} = info}, env, func_table, type_table, return_type) do
    {cond, _} = annotate_expr(cond, :bool, env, func_table, type_table)
    {else_block, _} = annotate_block(else_block, env, func_table, type_table, return_type)
    {{:guard, %{info | cond: cond, else_block: else_block}}, env}
  end

  defp annotate_stmt(stmt, env, _func_table, _type_table, _return_type), do: {stmt, env}

  defp annotate_else_branch(nil, _env, _func_table, _type_table, _return_type), do: nil
  defp annotate_else_branch({:else_block, %{block: block, span: span}}, env, func_table, type_table, return_type) do
    {block, _} = annotate_block(block, env, func_table, type_table, return_type)
    {:else_block, %{block: block, span: span}}
  end
  defp annotate_else_branch({:else_if, %{if: if_stmt, span: span}}, env, func_table, type_table, return_type) do
    {if_stmt, _} = annotate_stmt(if_stmt, env, func_table, type_table, return_type)
    {:else_if, %{if: if_stmt, span: span}}
  end

  defp annotate_expr({:struct, %{fields: fields, span: span} = info}, expected, env, func_table, type_table) do
    expected = normalize_type(expected)
    {field_types, struct_type} =
      case struct_type_info(expected) do
        {:ok, name, args} ->
          case Map.fetch(type_table, name) do
            {:ok, %{params: params, fields: field_map}} ->
              case type_arg_map(params, args) do
                {:ok, param_map} ->
                  {substitute_field_types(field_map, param_map), expected}

                :error ->
                  {%{}, nil}
              end

            :error ->
              {%{}, nil}
          end

        _ ->
          {%{}, nil}
      end

    fields =
      Enum.map(fields, fn %{name: name, expr: expr} = field ->
        expected_field = Map.get(field_types, name, nil)
        {expr, _} = annotate_expr(expr, expected_field, env, func_table, type_table)
        %{field | expr: expr}
      end)

    expr = {:struct, %{info | fields: fields, type: struct_type, span: span}}
    {expr, struct_type || expected}
  end

  defp annotate_expr({:call, %{name: name, args: args, span: span}}, _expected, env, func_table, type_table) do
    args =
      case Map.fetch(func_table, name) do
        {:ok, {param_types, _return_type}} ->
          args
          |> Enum.zip(param_types)
          |> Enum.map(fn {arg, expected_type} ->
            {arg, _} = annotate_expr(arg, expected_type, env, func_table, type_table)
            arg
          end)

        :error ->
          Enum.map(args, fn arg ->
            {arg, _} = annotate_expr(arg, nil, env, func_table, type_table)
            arg
          end)
      end

    {{:call, %{name: name, args: args, span: span}}, nil}
  end

  defp annotate_expr({:field, %{target: target, name: name, span: span}}, _expected, env, func_table, type_table) do
    {target, _} = annotate_expr(target, nil, env, func_table, type_table)
    {{:field, %{target: target, name: name, span: span}}, nil}
  end

  defp annotate_expr({:match, %{expr: expr, cases: cases, span: span}}, expected, env, func_table, type_table) do
    {expr, _} = annotate_expr(expr, nil, env, func_table, type_table)

    cases =
      Enum.map(cases, fn %{pattern: pattern, guard: guard, body: body, span: cspan} = match_case ->
        {body, _} =
          case body do
            {:block_expr, %{block: block, span: bspan}} ->
              {block, _} = annotate_block(block, env, func_table, type_table, expected)
              {{:block_expr, %{block: block, span: bspan}}, nil}

            _ ->
              annotate_expr(body, expected, env, func_table, type_table)
          end

        {guard, _} =
          case guard do
            nil -> {nil, nil}
            _ -> annotate_expr(guard, :bool, env, func_table, type_table)
          end

        %{match_case | pattern: pattern, guard: guard, body: body, span: cspan}
      end)

    {{:match, %{expr: expr, cases: cases, span: span}}, expected}
  end

  defp annotate_expr({:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch, span: span}}, expected, env, func_table, type_table) do
    {cond, _} = annotate_expr(cond, :bool, env, func_table, type_table)
    {then_block, _} = annotate_block(then_block, env, func_table, type_table, expected)
    else_branch = annotate_else_branch(else_branch, env, func_table, type_table, expected)
    {{:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch, span: span}}, expected}
  end

  defp annotate_expr({:block_expr, %{block: block, span: span}}, expected, env, func_table, type_table) do
    {block, _} = annotate_block(block, env, func_table, type_table, expected)
    {{:block_expr, %{block: block, span: span}}, expected}
  end

  defp annotate_expr({:binary, %{op: op, left: left, right: right, span: span}}, _expected, env, func_table, type_table) do
    {left, _} = annotate_expr(left, nil, env, func_table, type_table)
    {right, _} = annotate_expr(right, nil, env, func_table, type_table)
    {{:binary, %{op: op, left: left, right: right, span: span}}, nil}
  end

  defp annotate_expr({:opt_some, %{expr: expr, span: span}}, expected, env, func_table, type_table) do
    inner =
      case normalize_type(expected) do
        {:optional, inner} -> inner
        _ -> nil
      end

    {expr, _} = annotate_expr(expr, inner, env, func_table, type_table)
    {{:opt_some, %{expr: expr, span: span}}, expected}
  end

  defp annotate_expr({:res_ok, %{expr: expr, span: span}}, expected, env, func_table, type_table) do
    inner =
      case normalize_type(expected) do
        {:result, ok_type, _} -> ok_type
        _ -> nil
      end

    {expr, _} = annotate_expr(expr, inner, env, func_table, type_table)
    {{:res_ok, %{expr: expr, span: span}}, expected}
  end

  defp annotate_expr({:res_err, %{expr: expr, span: span}}, expected, env, func_table, type_table) do
    inner =
      case normalize_type(expected) do
        {:result, _, err_type} -> err_type
        _ -> nil
      end

    {expr, _} = annotate_expr(expr, inner, env, func_table, type_table)
    {{:res_err, %{expr: expr, span: span}}, expected}
  end

  defp annotate_expr(expr, _expected, _env, _func_table, _type_table), do: {expr, nil}

  defp normalize_type(nil), do: nil

  defp normalize_type({:generic, {:named, "Optional"}, [inner]}),
    do: {:optional, normalize_type(inner)}

  defp normalize_type({:generic, {:named, "Result"}, [ok_type, err_type]}),
    do: {:result, normalize_type(ok_type), normalize_type(err_type)}

  defp normalize_type({:generic, base, args}),
    do: {:generic, normalize_type(base), Enum.map(args, &normalize_type/1)}

  defp normalize_type({:optional, inner}), do: {:optional, normalize_type(inner)}

  defp normalize_type({:result, ok_type, err_type}),
    do: {:result, normalize_type(ok_type), normalize_type(err_type)}

  defp normalize_type(:any), do: :any

  defp normalize_type(type), do: type

  defp type_arg_map(params, args) do
    if length(params) == length(args) do
      {:ok, Map.new(Enum.zip(params, args))}
    else
      :error
    end
  end

  defp substitute_field_types(field_map, param_map) do
    Enum.reduce(field_map, %{}, fn {name, type}, acc ->
      Map.put(acc, name, substitute_type(type, param_map))
    end)
  end

  defp substitute_type({:named, name}, param_map) do
    case Map.fetch(param_map, name) do
      {:ok, type} -> type
      :error -> {:named, name}
    end
  end

  defp substitute_type({:generic, base, args}, param_map) do
    {:generic, substitute_type(base, param_map), Enum.map(args, &substitute_type(&1, param_map))}
  end

  defp substitute_type({:optional, inner}, param_map) do
    {:optional, substitute_type(inner, param_map)}
  end

  defp substitute_type({:result, ok_type, err_type}, param_map) do
    {:result, substitute_type(ok_type, param_map), substitute_type(err_type, param_map)}
  end

  defp substitute_type(type, _param_map), do: type

  @spec type_label(BeamLang.AST.type_name()) :: binary()
  defp type_label({:generic, base, args}) do
    "#{type_label(base)}<#{Enum.map_join(args, ", ", &type_label/1)}>"
  end
  defp type_label({:named, name}) when is_binary(name), do: name
  defp type_label(:any), do: "any"
  defp type_label(:number), do: "number"
  defp type_label(:char), do: "char"
  defp type_label(:integer), do: "number"
  defp type_label(:float), do: "number"
  defp type_label({:optional, inner}), do: "#{type_label(inner)}?"
  defp type_label({:result, ok_type, err_type}), do: "#{type_label(ok_type)}!#{type_label(err_type)}"
  defp type_label(type) when is_atom(type), do: Atom.to_string(type)
end
