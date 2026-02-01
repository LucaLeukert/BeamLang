defmodule BeamLang.Semantic do
  @moduledoc """
  Performs basic semantic checks for the MVP.
  """

  @spec validate(BeamLang.AST.t(), keyword()) ::
          {:ok, BeamLang.AST.t()} | {:error, [BeamLang.Error.t()]}
  def validate(ast, opts \\ [])

  def validate({:program, %{types: types, errors: errors, functions: functions}} = ast, opts)
      when is_list(functions) do
    require_main = Keyword.get(opts, :require_main, true)
    {:ok, func_table} = build_function_table(functions)
    # Convert error_def to type_def for type checking (errors are just structs)
    error_types = Enum.map(errors, &error_def_to_type_def/1)
    all_types = types ++ error_types
    {:ok, type_table} = build_type_table(all_types)

    validation_errors =
      if(require_main, do: collect_main_error(functions), else: []) ++
        collect_type_errors(types, type_table) ++
        collect_function_errors(functions, func_table, type_table)

    case validation_errors do
      [] -> {:ok, annotate_program(ast, func_table, type_table)}
      _ -> {:error, validation_errors}
    end
  end

  # Also handle programs without errors field (for backwards compatibility)
  def validate({:program, %{functions: functions} = prog}, opts)
      when is_list(functions) and not is_map_key(prog, :errors) do
    validate({:program, Map.put(prog, :errors, [])}, opts)
  end

  # Convert error_def to type_def for type checking purposes
  defp error_def_to_type_def({:error_def, %{name: name, fields: fields, exported: exported, span: span}}) do
    {:type_def, %{name: name, params: [], fields: fields, exported: exported, span: span}}
  end

  @spec require_main_exists([BeamLang.AST.func()]) :: :ok | {:error, [BeamLang.Error.t()]}
  defp require_main_exists(functions) do
    case Enum.find(functions, fn {:function, %{name: name, body: body}} ->
           name == "main" and body != nil
         end) do
      nil ->
        span = BeamLang.Span.new("<source>", 0, 0)
        {:error, [BeamLang.Error.new(:type, "Missing required function 'main'.", span)]}

      {:function, %{params: params, span: span}} ->
        require_main_params(params, span)
    end
  end

  @spec require_main_params([BeamLang.AST.func_param()], BeamLang.Span.t()) ::
          :ok | {:error, [BeamLang.Error.t()]}
  defp require_main_params([%{name: "args", type: type}], _span) do
    if is_string_list_type?(type) do
      :ok
    else
      span = BeamLang.Span.new("<source>", 0, 0)
      {:error,
       [
         BeamLang.Error.new(
           :type,
           "main parameter 'args' must have type [String], got #{type_label(type)}.",
           span
         )
       ]}
    end
  end

  defp require_main_params(params, _span) do
    span = BeamLang.Span.new("<source>", 0, 0)

    cond do
      length(params) == 0 ->
        {:error,
         [
           BeamLang.Error.new(
             :type,
             "main must have exactly one parameter 'args: [String]'.",
             span
           )
         ]}

      length(params) > 1 ->
        {:error,
         [
           BeamLang.Error.new(
             :type,
             "main must have exactly one parameter 'args: [String]', got #{length(params)} parameters.",
             span
           )
         ]}

      true ->
        [%{name: name}] = params

        {:error,
         [
           BeamLang.Error.new(
             :type,
             "main parameter must be named 'args', got '#{name}'.",
             span
           )
         ]}
    end
  end

  @spec is_string_list_type?(BeamLang.AST.type_name()) :: boolean()
  defp is_string_list_type?({:generic, {:named, "List"}, [:String]}), do: true
  defp is_string_list_type?(_), do: false

  @spec typecheck_return(BeamLang.AST.type_name(), BeamLang.AST.block(), map(), map(), map()) ::
          :ok | {:error, [BeamLang.Error.t()]}
  defp typecheck_return(
         expected_type,
         {:block, %{stmts: stmts}} = block,
         func_table,
         type_table,
         env
       ) do
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
            {:error,
             [
               BeamLang.Error.new(
                 :type,
                 "Unknown function in return expression.",
                 expr_span(expr)
               )
             ]}

          {:error, :wrong_arity} ->
            {:error,
             [
               BeamLang.Error.new(
                 :type,
                 "Function called with wrong number of arguments.",
                 expr_span(expr)
               )
             ]}

          {:error, :unknown_variable} ->
            {:error,
             [
               BeamLang.Error.new(
                 :type,
                 "Unknown variable in return expression.",
                 expr_span(expr)
               )
             ]}

          {:error, :unknown_type} ->
            {:error,
             [BeamLang.Error.new(:type, "Unknown type in return expression.", expr_span(expr))]}

          {:error, :internal_function} ->
            {:error,
             [
               BeamLang.Error.new(
                 :type,
                 "Internal function cannot be called outside its module.",
                 expr_span(expr)
               )
             ]}

          {:error, :missing_type_annotation} ->
            {:error,
             [
               BeamLang.Error.new(
                 :type,
                 "Struct literal requires a type annotation.",
                 expr_span(expr)
               )
             ]}

          {:error, :not_a_struct} ->
            {:error,
             [BeamLang.Error.new(:type, "Field access requires a struct value.", expr_span(expr))]}

          {:error, {:internal_field, name}} ->
            {:error, [BeamLang.Error.new(:type, "Field '#{name}' is internal and cannot be accessed directly.", expr_span(expr))]}

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
            {:error, [BeamLang.Error.new(:type, "Missing return statement.", block_span(stmts))]}
          end
        end
    end
  end

  @spec require_main_number(binary(), BeamLang.AST.type_name()) ::
          :ok | {:error, [BeamLang.Error.t()]}
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
             | :internal_function
             | :missing_type_annotation
             | :missing_optional_context
             | :missing_result_context
             | {:struct, [BeamLang.Error.t()]}
             | {:match, [BeamLang.Error.t()]}
             | {:call, [BeamLang.Error.t()]}
             | :invalid_binary_op
             | {:unknown_field, binary()}
             | :not_a_struct}
  defp type_of_expr({:integer, %{value: _value}}, _func_table, _type_table, _env, _expected),
    do: {:ok, :number}

  defp type_of_expr({:float, %{value: _value}}, _func_table, _type_table, _env, _expected),
    do: {:ok, :number}

  defp type_of_expr({:string, %{value: _value}}, _func_table, _type_table, _env, _expected),
    do: {:ok, :String}

  defp type_of_expr(
         {:interpolated_string, %{expressions: expressions}},
         func_table,
         type_table,
         env,
         _expected
       ) do
    # Validate that all expressions in the interpolation are valid
    errors =
      Enum.flat_map(expressions, fn expr ->
        case type_of_expr(expr, func_table, type_table, env, nil) do
          {:ok, _type} ->
            # Any type can be interpolated into a string
            []

          {:error, reason} ->
            expr_error(reason, expr)
        end
      end)

    if errors == [] do
      {:ok, :String}
    else
      {:error, {:match, errors}}
    end
  end

  defp type_of_expr({:char, %{value: _value}}, _func_table, _type_table, _env, _expected),
    do: {:ok, :char}

  defp type_of_expr({:bool, %{value: _value}}, _func_table, _type_table, _env, _expected),
    do: {:ok, :bool}

  defp type_of_expr(
         {:lambda, %{params: params, return_type: return_type, body: body}},
         func_table,
         type_table,
         env,
         _expected
       ) do
    lambda_errors = validate_param_names(params)

    param_env =
      Enum.reduce(params, %{}, fn param, acc ->
        param_mutable = Map.get(param, :mutable, false)
        Map.put(acc, param.name, %{type: normalize_type(param.type), mutable: param_mutable})
      end)

    return_type = normalize_type(return_type)
    lambda_env = Map.merge(env, param_env)

    {:ok, _env, stmt_errors} =
      validate_statements(body, func_table, type_table, lambda_env, return_type)

    errors =
      case typecheck_return(return_type, body, func_table, type_table, lambda_env) do
        :ok -> stmt_errors ++ lambda_errors
        {:error, errs} -> stmt_errors ++ errs ++ lambda_errors
      end

    if errors == [] do
      {:ok, {:fn, Enum.map(params, &normalize_type(&1.type)), return_type}}
    else
      {:error, {:match, errors}}
    end
  end

  defp type_of_expr(
         {:method_call, %{target: target, name: name, args: args}},
         func_table,
         type_table,
         env,
         _expected
       ) do
    case type_of_expr(target, func_table, type_table, env, nil) do
      {:ok, target_type} ->
        case method_call_type(
               normalize_type(target_type),
               name,
               args,
               func_table,
               type_table,
               env
             ) do
          {:ok, type} -> {:ok, type}
          {:error, {:match, errors}} -> {:error, {:match, errors}}
          {:error, err} -> {:error, {:match, [err]}}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp type_of_expr(
         {:call, %{name: name, args: args, span: call_span} = info},
         func_table,
         type_table,
         env,
         _expected
       ) do
    type_args = Map.get(info, :type_args, [])

    parse_args_errors =
      if name == "parse_args" do
        parse_args_type_errors(type_args, type_table, call_span)
      else
        []
      end

    if parse_args_errors != [] do
      {:error, {:call, parse_args_errors}}
    else
      case Map.fetch(func_table, name) do
        {:ok,
         %{
           params: param_types,
           return: return_type,
           internal: internal,
           span: func_span,
           type_params: type_params
         }} ->
          if internal_call?(internal, func_span, call_span) do
            {:error, :internal_function}
          else
            if length(param_types) != length(args) do
              {:error, :wrong_arity}
            else
              case type_args do
                [] ->
                  if type_params != [] do
                    case infer_type_params(param_types, args, func_table, type_table, env) do
                      {:ok, mapping} ->
                        {:ok, resolved} = substitute_type_vars(return_type, mapping, call_span)
                        {:ok, resolved}

                      {:error, errors} ->
                        {:error, {:call, errors}}
                    end
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

                _ ->
                  if type_params == [] do
                    {:error,
                     {:call,
                      [
                        BeamLang.Error.new(
                          :type,
                          "Type arguments provided to non-generic function.",
                          call_span
                        )
                      ]}}
                  else
                    normalized_args = Enum.map(type_args, &normalize_type/1)

                    case type_arg_map(type_params, normalized_args) do
                      :error ->
                        {:error,
                         {:call,
                          [
                            BeamLang.Error.new(
                              :type,
                              "Type argument count mismatch. Expected #{length(type_params)}, got #{length(normalized_args)}.",
                              call_span
                            )
                          ]}}

                      {:ok, mapping} ->
                        param_types = Enum.map(param_types, &substitute_type(&1, mapping))
                        return_type = substitute_type(return_type, mapping)

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
                  end
              end
            end
          end

        :error ->
          case Map.fetch(env, name) do
            {:ok, %{type: {:fn, param_types, return_type}}} ->
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

            _ ->
              {:error, :unknown_function}
          end
      end
    end
  end

  defp type_of_expr(
         {:identifier, %{name: name, span: ident_span}},
         func_table,
         _type_table,
         env,
         _expected
       ) do
    case Map.fetch(env, name) do
      {:ok, %{type: type}} ->
        {:ok, type}

      :error ->
        case Map.fetch(func_table, name) do
          {:ok, %{params: param_types, return: return_type, internal: internal, span: func_span}} ->
            if internal_call?(internal, func_span, ident_span) do
              {:error, :internal_function}
            else
              {:ok, {:fn, param_types, return_type}}
            end

          :error ->
            {:error, :unknown_variable}
        end
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

                errors =
                  validate_struct_fields(
                    fields,
                    type_fields,
                    func_table,
                    type_table,
                    env,
                    type_name
                  )

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

      nil ->
        case type_of_expr(expr, func_table, type_table, env, nil) do
          {:ok, inferred} -> {:ok, {:optional, inferred}}
          {:error, reason} -> {:error, reason}
        end

      _ ->
        {:error, :missing_optional_context}
    end
  end

  defp type_of_expr({:opt_none, %{span: _span}}, _func_table, _type_table, _env, expected) do
    case expected do
      {:optional, inner} -> {:ok, {:optional, inner}}
      nil -> {:ok, {:optional, :any}}
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

      nil ->
        case type_of_expr(expr, func_table, type_table, env, nil) do
          {:ok, inferred} -> {:ok, {:result, inferred, :any}}
          {:error, reason} -> {:error, reason}
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

      nil ->
        case type_of_expr(expr, func_table, type_table, env, nil) do
          {:ok, inferred} -> {:ok, {:result, :any, inferred}}
          {:error, reason} -> {:error, reason}
        end

      _ ->
        {:error, :missing_result_context}
    end
  end

  defp type_of_expr({:list_literal, %{elements: elements, span: _span}}, func_table, type_table, env, expected) do
    elem_type =
      case expected do
        {:generic, {:named, "List"}, [t]} -> t
        _ -> :any
      end

    errors =
      Enum.flat_map(elements, fn elem ->
        case type_of_expr(elem, func_table, type_table, env, elem_type) do
          {:ok, inferred} ->
            if elem_type == :any or type_compatible?(elem_type, inferred) do
              []
            else
              [
                BeamLang.Error.new(
                  :type,
                  "List element type mismatch. Expected #{type_label(elem_type)}, got #{type_label(inferred)}.",
                  expr_span(elem)
                )
              ]
            end

          {:error, reason} when is_binary(reason) ->
            [BeamLang.Error.new(:type, reason, expr_span(elem))]

          {:error, %BeamLang.Error{} = err} ->
            [err]

          {:error, {:match, errs}} ->
            errs

          {:error, _reason} ->
            [BeamLang.Error.new(:type, "Invalid list element.", expr_span(elem))]
        end
      end)

    if errors == [] do
      inferred_elem_type =
        case elements do
          [] -> elem_type
          [first | _] ->
            case type_of_expr(first, func_table, type_table, env, nil) do
              {:ok, t} -> t
              _ -> :any
            end
        end

      {:ok, {:generic, {:named, "List"}, [inferred_elem_type]}}
    else
      {:error, {:match, errors}}
    end
  end

  defp type_of_expr({:range, %{start: start_expr, end: end_expr, span: span}}, func_table, type_table, env, _expected) do
    with {:ok, start_type} <- type_of_expr(start_expr, func_table, type_table, env, :number),
         {:ok, end_type} <- type_of_expr(end_expr, func_table, type_table, env, :number) do
      if start_type == :number and end_type == :number do
        {:ok, :range}
      else
        {:error, BeamLang.Error.new(:type, "Range bounds must be numbers.", span)}
      end
    end
  end

  defp type_of_expr(
         {:field, %{target: target, name: name}},
         func_table,
         type_table,
         env,
         _expected
       ) do
    case type_of_expr(target, func_table, type_table, env, nil) do
      {:ok, type} ->
        case struct_type_info(type) do
          {:ok, type_name, args} ->
            # Allow internal field access if:
            # 1. Accessing via 'self', OR
            # 2. We're in a method of the same type (method_type_context matches target type)
            is_self_access = match?({:identifier, %{name: "self"}}, target)
            method_type = Map.get(env, :__method_type__)
            is_same_type_method = method_type != nil and method_type == type_name

            case Map.fetch(type_table, type_name) do
              {:ok, %{params: params, fields: field_map}} ->
                case type_arg_map(params, args) do
                  {:ok, param_map} ->
                    type_fields = substitute_field_types(field_map, param_map)
                    can_access_internal = is_self_access or is_same_type_method

                    case Map.fetch(type_fields, name) do
                      {:ok, %{type: _field_type, internal: true}} when not can_access_internal ->
                        {:error, {:internal_field, name}}

                      {:ok, %{type: field_type, internal: _}} ->
                        {:ok, field_type}

                      :error ->
                        {:error, {:unknown_field, name}}
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

  defp type_of_expr(
         {:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch}},
         func_table,
         type_table,
         env,
         _expected
       ) do
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

  defp type_of_expr(
         {:match, %{expr: expr, cases: cases, span: span}},
         func_table,
         type_table,
         env,
         _expected
       ) do
    case type_of_expr(expr, func_table, type_table, env, nil) do
      {:ok, match_type} ->
        {case_types, errors} =
          Enum.reduce(cases, {[], []}, fn %{pattern: pattern, guard: guard, body: body},
                                          {types, errs} ->
            {bindings, pattern_errors} = pattern_bindings(pattern, match_type, type_table)
            case_env = Map.merge(env, bindings)
            guard_errors = guard_errors(guard, func_table, type_table, case_env)
            {body_type, body_errors} = case_body_type(body, func_table, type_table, case_env)

            {[{body_type, expr_span(body)} | types],
             errs ++ pattern_errors ++ guard_errors ++ body_errors}
          end)

        case_types = Enum.reverse(case_types)

        errors = errors ++ non_exhaustive_match_errors(match_type, cases, span)

        if errors != [] do
          {:error, {:match, errors}}
        else
          case case_types do
            [] ->
              {:error,
               {:match,
                [
                  BeamLang.Error.new(
                    :type,
                    "Match expression must have at least one case.",
                    BeamLang.Span.new("<source>", 0, 0)
                  )
                ]}}

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

  defp type_of_expr(
         {:binary, %{op: op, left: left, right: right}},
         func_table,
         type_table,
         env,
         _expected
       ) do
    with {:ok, left_type} <- type_of_expr(left, func_table, type_table, env, nil),
         {:ok, right_type} <- type_of_expr(right, func_table, type_table, env, nil) do
      # First check if left type has operator overload
      case check_operator_overload(op, left_type, right_type, type_table, func_table) do
        {:ok, result_type, _func_name} ->
          {:ok, result_type}

        :no_overload ->
          # Fall back to built-in operators
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
  end

  @spec check_operator_overload(atom(), BeamLang.AST.type_name(), BeamLang.AST.type_name(), map(), map()) ::
          {:ok, BeamLang.AST.type_name(), binary()} | :no_overload
  defp check_operator_overload(op, left_type, right_type, type_table, func_table) do
    case get_type_name(left_type) do
      {:ok, type_name} ->
        case Map.fetch(type_table, type_name) do
          {:ok, %{operators: operators}} when is_map(operators) ->
            case Map.fetch(operators, op) do
              {:ok, func_names} ->
                # Find matching operator function
                find_matching_operator(func_names, left_type, right_type, func_table)

              :error ->
                :no_overload
            end

          _ ->
            :no_overload
        end

      :error ->
        :no_overload
    end
  end

  @spec find_matching_operator([binary()], BeamLang.AST.type_name(), BeamLang.AST.type_name(), map()) ::
          {:ok, BeamLang.AST.type_name(), binary()} | :no_overload
  defp find_matching_operator([], _left_type, _right_type, _func_table), do: :no_overload

  defp find_matching_operator([func_name | rest], left_type, right_type, func_table) do
    case Map.fetch(func_table, func_name) do
      {:ok, func_info} ->
        # params is a list of normalized types, not maps
        params = func_info.params
        if length(params) == 2 do
          [param1_type, param2_type] = params
          if types_match?(param1_type, normalize_type(left_type)) and
               types_match?(param2_type, normalize_type(right_type)) do
            {:ok, func_info.return, func_name}
          else
            find_matching_operator(rest, left_type, right_type, func_table)
          end
        else
          find_matching_operator(rest, left_type, right_type, func_table)
        end

      :error ->
        find_matching_operator(rest, left_type, right_type, func_table)
    end
  end

  @spec get_type_name(BeamLang.AST.type_name()) :: {:ok, binary()} | :error
  defp get_type_name({:named, name}), do: {:ok, name}
  defp get_type_name({:generic, {:named, name}, _args}), do: {:ok, name}
  defp get_type_name(_), do: :error

  defp non_exhaustive_match_errors(match_type, cases, span) do
    if match_exhaustive?(match_type, cases) do
      []
    else
      [
        BeamLang.Error.new(
          :type,
          "Non-exhaustive match. Add a 'case _' or cover all variants.",
          span
        )
      ]
    end
  end

  defp match_exhaustive?(match_type, cases) do
    patterns = Enum.map(cases, & &1.pattern)

    if Enum.any?(patterns, &wildcard_pattern?/1) do
      true
    else
      match_exhaustive_for_type?(normalize_type(match_type), patterns)
    end
  end

  defp wildcard_pattern?({:wildcard, _}), do: true
  defp wildcard_pattern?(_), do: false

  defp match_exhaustive_for_type?({:optional, _inner}, patterns) do
    has_some = Enum.any?(patterns, &match?({:opt_some_pat, _}, &1))
    has_none = Enum.any?(patterns, &match?({:opt_none_pat, _}, &1))
    has_some and has_none
  end

  defp match_exhaustive_for_type?({:result, _ok_type, _err_type}, patterns) do
    has_ok = Enum.any?(patterns, &match?({:res_ok_pat, _}, &1))
    has_err = Enum.any?(patterns, &match?({:res_err_pat, _}, &1))
    has_ok and has_err
  end

  defp match_exhaustive_for_type?(:bool, patterns) do
    has_true = Enum.any?(patterns, &match?({:bool, %{value: true}}, &1))
    has_false = Enum.any?(patterns, &match?({:bool, %{value: false}}, &1))
    has_true and has_false
  end

  defp match_exhaustive_for_type?({:named, "bool"}, patterns) do
    match_exhaustive_for_type?(:bool, patterns)
  end

  defp match_exhaustive_for_type?(_type, _patterns), do: false

  defp parse_args_type_errors(type_args, type_table, span) do
    case parse_args_type_info(type_args, type_table, span) do
      {:ok, _info} -> []
      {:error, errors} -> errors
    end
  end

  defp parse_args_type_info([type_arg], type_table, span) do
    case struct_type_info(type_arg) do
      {:ok, type_name, type_args} ->
        case Map.fetch(type_table, type_name) do
          {:ok, %{params: params, fields: fields, field_order: field_order}} ->
            case type_arg_map(params, type_args) do
              :error ->
                {:error,
                 [
                   BeamLang.Error.new(
                     :type,
                     "Type argument count mismatch. Expected #{length(params)}, got #{length(type_args)}.",
                     span
                   )
                 ]}

              {:ok, param_map} ->
                field_types =
                  Enum.map(field_order, fn name ->
                    %{type: type} = Map.fetch!(fields, name)
                    substitute_type(type, param_map)
                  end)

                if Enum.all?(field_types, &parse_args_field_type?/1) do
                  {:ok, %{type: type_arg, fields: field_order, field_types: field_types}}
                else
                  {:error,
                   [
                     BeamLang.Error.new(
                        :type,
                        "parse_args only supports String, number, bool, and char fields.",
                        span
                      )
                    ]}
                end
            end

          :error ->
            {:error,
             [
               BeamLang.Error.new(
                 :type,
                 "Unknown type in parse_args.",
                 span
               )
             ]}
        end

      _ ->
        {:error,
         [
           BeamLang.Error.new(
             :type,
             "parse_args requires a struct type argument.",
             span
           )
         ]}
    end
  end

  defp parse_args_type_info([], _type_table, span) do
    {:error,
     [
       BeamLang.Error.new(
         :type,
         "parse_args requires a type argument.",
         span
       )
     ]}
  end

  defp parse_args_type_info(_other, _type_table, span) do
    {:error,
     [
       BeamLang.Error.new(
         :type,
         "parse_args expects a single type argument.",
         span
       )
     ]}
  end

  defp parse_args_field_type?(:String), do: true
  defp parse_args_field_type?({:named, "String"}), do: true
  defp parse_args_field_type?(:number), do: true
  defp parse_args_field_type?({:named, "number"}), do: true
  defp parse_args_field_type?(:bool), do: true
  defp parse_args_field_type?({:named, "bool"}), do: true
  defp parse_args_field_type?(:char), do: true
  defp parse_args_field_type?({:named, "char"}), do: true
  defp parse_args_field_type?(_), do: false

  defp method_call_type(target_type, name, args, func_table, type_table, env) do
    error_span =
      case args do
        [first | _] -> expr_span(first)
        _ -> BeamLang.Span.new("<source>", 0, 0)
      end

    case struct_type_info(target_type) do
      {:ok, type_name, type_args} ->
        case Map.fetch(type_table, type_name) do
          {:ok, %{params: params, fields: field_map}} ->
            case type_arg_map(params, type_args) do
              {:ok, param_map} ->
                type_fields = substitute_field_types(field_map, param_map)

                case Map.fetch(type_fields, name) do
                  {:ok, %{type: {:fn, [self_type | param_types], return_type}}} ->
                    if type_compatible?(self_type, target_type) do
                      if length(param_types) != length(args) do
                        {:error,
                         BeamLang.Error.new(
                           :type,
                           "Method '#{name}' called with wrong number of arguments.",
                           error_span
                         )}
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

                        if errors == [], do: {:ok, return_type}, else: {:error, {:match, errors}}
                      end
                    else
                      {:error,
                       BeamLang.Error.new(
                         :type,
                         "Method '#{name}' does not match receiver type.",
                         error_span
                       )}
                    end

                  {:ok, %{type: _field_type}} ->
                    {:error,
                     BeamLang.Error.new(
                       :type,
                       "Field '#{name}' is not callable.",
                       BeamLang.Span.new("<source>", 0, 0)
                     )}

                  :error ->
                    {:error,
                     BeamLang.Error.new(
                       :type,
                       "Unknown field '#{name}'.",
                       BeamLang.Span.new("<source>", 0, 0)
                     )}
                end

              :error ->
                {:error,
                 BeamLang.Error.new(
                   :type,
                   "Unknown type in method call.",
                   BeamLang.Span.new("<source>", 0, 0)
                 )}
            end

          :error ->
            {:error,
             BeamLang.Error.new(
               :type,
               "Unknown type in method call.",
               BeamLang.Span.new("<source>", 0, 0)
             )}
        end

      :missing ->
        {:error,
         BeamLang.Error.new(
           :type,
           "Method call requires a struct value.",
           BeamLang.Span.new("<source>", 0, 0)
         )}

      :error ->
        {:error,
         BeamLang.Error.new(
           :type,
           "Method call requires a struct value.",
           BeamLang.Span.new("<source>", 0, 0)
         )}
    end
  end

  @spec validate_function(BeamLang.AST.func(), map(), map()) :: [BeamLang.Error.t()]
  defp validate_function(
         {:function,
          %{
            name: name,
            type_params: type_params,
            params: params,
            return_type: return_type,
            body: body
          }},
         func_table,
         type_table
       ) do
    errors = validate_param_names(params)

    param_env =
      Enum.reduce(params, %{}, fn %{name: param_name, type: param_type, mutable: mutable}, acc ->
        param_type = param_type |> normalize_type() |> replace_type_params(type_params)
        Map.put(acc, param_name, %{type: param_type, mutable: mutable})
      end)

    # If first param is 'self', track the method type context for internal field access
    param_env = add_method_type_context(param_env, params)

    return_type = return_type |> normalize_type() |> replace_type_params(type_params)

    {env, stmt_errors} =
      case body do
        nil ->
          {param_env, []}

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

    dedupe_errors(errors)
  end

  defp dedupe_errors(errors) do
    Enum.uniq_by(errors, fn %BeamLang.Error{kind: kind, message: message, span: span} ->
      {kind, message, span.file_id, span.start, span.end}
    end)
  end

  @spec validate_statements(BeamLang.AST.block(), map(), map(), map()) ::
          {:ok, map(), [BeamLang.Error.t()]}
  defp validate_statements(block, func_table, type_table, env) do
    validate_statements(block, func_table, type_table, env, 0, nil)
  end

  @spec validate_statements(
          BeamLang.AST.block(),
          map(),
          map(),
          map(),
          BeamLang.AST.type_name() | nil
        ) ::
          {:ok, map(), [BeamLang.Error.t()]}
  defp validate_statements(block, func_table, type_table, env, return_type) do
    validate_statements(block, func_table, type_table, env, 0, return_type)
  end

  @spec validate_statements(
          BeamLang.AST.block(),
          map(),
          map(),
          map(),
          non_neg_integer(),
          BeamLang.AST.type_name() | nil
        ) ::
          {:ok, map(), [BeamLang.Error.t()]}
  defp validate_statements(
         {:block, %{stmts: stmts}},
         func_table,
         type_table,
         env,
         loop_depth,
         return_type
       ) do
    Enum.reduce_while(stmts, {:ok, env, []}, fn stmt, {:ok, acc_env, errors} ->
      case stmt do
        {:break, %{span: span}} ->
          if loop_depth == 0 do
            {:cont,
             {:ok, acc_env,
              [BeamLang.Error.new(:type, "break used outside of loop.", span) | errors]}}
          else
            {:cont, {:ok, acc_env, errors}}
          end

        {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}} ->
          errors = errors ++ condition_errors(cond, func_table, type_table, acc_env)

          {:ok, _env_then, then_errors} =
            validate_statements(
              then_block,
              func_table,
              type_table,
              acc_env,
              loop_depth,
              return_type
            )

          errors = then_errors ++ errors

          {errors, _} =
            else_errors(
              else_branch,
              func_table,
              type_table,
              acc_env,
              loop_depth,
              return_type,
              errors
            )

          {:cont, {:ok, acc_env, errors}}

        {:while, %{cond: cond, body: body}} ->
          errors = errors ++ condition_errors(cond, func_table, type_table, acc_env)

          {:ok, _env_body, body_errors} =
            validate_statements(
              body,
              func_table,
              type_table,
              acc_env,
              loop_depth + 1,
              return_type
            )

          {:cont, {:ok, acc_env, body_errors ++ errors}}

        {:loop, %{body: body}} ->
          {:ok, _env_body, body_errors} =
            validate_statements(
              body,
              func_table,
              type_table,
              acc_env,
              loop_depth + 1,
              return_type
            )

          {:cont, {:ok, acc_env, body_errors ++ errors}}

        {:for, %{name: name, collection: collection, body: body, span: span}} ->
          errors =
            if name == "self" do
              [BeamLang.Error.new(:type, "self is reserved for method receivers.", span) | errors]
            else
              errors
            end

          errors = errors ++ iterator_errors(collection, func_table, type_table, acc_env, span)
          item_type = iterator_item_type(collection, func_table, type_table, acc_env)
          loop_env = Map.put(acc_env, name, %{type: item_type, mutable: false})

          {:ok, _env_body, body_errors} =
            validate_statements(
              body,
              func_table,
              type_table,
              loop_env,
              loop_depth + 1,
              return_type
            )

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
            validate_statements(
              else_block,
              func_table,
              type_table,
              acc_env,
              loop_depth,
              return_type
            )

          errors = else_errors ++ errors

          errors =
            if guard_has_return?(else_block) do
              errors
            else
              [
                BeamLang.Error.new(
                  :type,
                  "Guard else block must end with return.",
                  block_span(else_block)
                )
                | errors
              ]
            end

          {:cont, {:ok, acc_env, errors}}

        {:let, %{name: name, expr: expr, mutable: mutable, type: declared_type}} ->
          declared_type = normalize_type(declared_type)

          errors =
            if name == "self" do
              [
                BeamLang.Error.new(
                  :type,
                  "self is reserved for method receivers.",
                  stmt_span(stmt)
                )
                | errors
              ]
            else
              errors
            end

          case type_of_expr(expr, func_table, type_table, acc_env, declared_type) do
            {:error, :unknown_function} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Unknown function in let expression.",
                    expr_span(expr)
                  )
                  | errors
                ]}}

            {:error, :wrong_arity} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Function called with wrong number of arguments.",
                    expr_span(expr)
                  )
                  | errors
                ]}}

            {:error, :unknown_variable} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Unknown variable in let expression.",
                    expr_span(expr)
                  )
                  | errors
                ]}}

            {:error, :unknown_type} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(:type, "Unknown type in let expression.", expr_span(expr))
                  | errors
                ]}}

            {:error, :internal_function} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Internal function cannot be called outside its module.",
                    expr_span(expr)
                  )
                  | errors
                ]}}

            {:error, :missing_type_annotation} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Struct literal requires a type annotation.",
                    expr_span(expr)
                  )
                  | errors
                ]}}

            {:error, :not_a_struct} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Field access requires a struct value.",
                    expr_span(expr)
                  )
                  | errors
                ]}}

            {:error, {:internal_field, field}} ->
              {:cont,
               {:ok, acc_env,
                [BeamLang.Error.new(:type, "Field '#{field}' is internal and cannot be accessed directly.", expr_span(expr)) | errors]}}

            {:error, {:unknown_field, field}} ->
              {:cont,
               {:ok, acc_env,
                [BeamLang.Error.new(:type, "Unknown field '#{field}'.", expr_span(expr)) | errors]}}

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
                    BeamLang.Error.new(
                      :type,
                      "Variable '#{name}' is already defined.",
                      stmt_span(stmt)
                    )
                    | errors
                  ]}}
              else
                {:cont,
                 {:ok, Map.put(acc_env, name, %{type: inferred, mutable: mutable}), errors}}
              end
          end

        {:assign, %{target: target, expr: expr}} ->
          case assignment_target(target, acc_env) do
            {:error, err} ->
              {:cont, {:ok, acc_env, [err | errors]}}

            {:ok, %{name: name, type: _declared, mutable: false}} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Cannot assign to immutable variable '#{name}'.",
                    stmt_span(stmt)
                  )
                  | errors
                ]}}

            {:ok, %{name: _name, type: declared, mutable: true, field: nil}} ->
              case type_of_expr(expr, func_table, type_table, acc_env, declared) do
                {:error, :unknown_function} ->
                  {:cont,
                   {:ok, acc_env,
                    [
                      BeamLang.Error.new(
                        :type,
                        "Unknown function in assignment.",
                        expr_span(expr)
                      )
                      | errors
                    ]}}

                {:error, :wrong_arity} ->
                  {:cont,
                   {:ok, acc_env,
                    [
                      BeamLang.Error.new(
                        :type,
                        "Function called with wrong number of arguments.",
                        expr_span(expr)
                      )
                      | errors
                    ]}}

                {:error, :unknown_variable} ->
                  {:cont,
                   {:ok, acc_env,
                    [
                      BeamLang.Error.new(
                        :type,
                        "Unknown variable in assignment.",
                        expr_span(expr)
                      )
                      | errors
                    ]}}

                {:error, :unknown_type} ->
                  {:cont,
                   {:ok, acc_env,
                    [
                      BeamLang.Error.new(:type, "Unknown type in assignment.", expr_span(expr))
                      | errors
                    ]}}

                {:error, :internal_function} ->
                  {:cont,
                   {:ok, acc_env,
                    [
                      BeamLang.Error.new(
                        :type,
                        "Internal function cannot be called outside its module.",
                        expr_span(expr)
                      )
                      | errors
                    ]}}

                {:error, :missing_type_annotation} ->
                  {:cont,
                   {:ok, acc_env,
                    [
                      BeamLang.Error.new(
                        :type,
                        "Struct literal requires a type annotation.",
                        expr_span(expr)
                      )
                      | errors
                    ]}}

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

            {:ok, %{name: var_name, type: declared, mutable: true, field: field}} ->
              is_self_access = var_name == "self"
              method_type = Map.get(acc_env, :__method_type__)
              case resolve_field_type(declared, field, type_table, is_self_access, method_type) do
                {:error, err} ->
                  {:cont, {:ok, acc_env, [err | errors]}}

                {:ok, field_type} ->
                  case type_of_expr(expr, func_table, type_table, acc_env, field_type) do
                    {:error, :unknown_function} ->
                      {:cont,
                       {:ok, acc_env,
                        [
                          BeamLang.Error.new(
                            :type,
                            "Unknown function in assignment.",
                            expr_span(expr)
                          )
                          | errors
                        ]}}

                    {:error, :wrong_arity} ->
                      {:cont,
                       {:ok, acc_env,
                        [
                          BeamLang.Error.new(
                            :type,
                            "Function called with wrong number of arguments.",
                            expr_span(expr)
                          )
                          | errors
                        ]}}

                    {:error, :unknown_variable} ->
                      {:cont,
                       {:ok, acc_env,
                        [
                          BeamLang.Error.new(
                            :type,
                            "Unknown variable in assignment.",
                            expr_span(expr)
                          )
                          | errors
                        ]}}

                    {:error, :unknown_type} ->
                      {:cont,
                       {:ok, acc_env,
                        [
                          BeamLang.Error.new(
                            :type,
                            "Unknown type in assignment.",
                            expr_span(expr)
                          )
                          | errors
                        ]}}

                    {:error, :internal_function} ->
                      {:cont,
                       {:ok, acc_env,
                        [
                          BeamLang.Error.new(
                            :type,
                            "Internal function cannot be called outside its module.",
                            expr_span(expr)
                          )
                          | errors
                        ]}}

                    {:error, :missing_type_annotation} ->
                      {:cont,
                       {:ok, acc_env,
                        [
                          BeamLang.Error.new(
                            :type,
                            "Struct literal requires a type annotation.",
                            expr_span(expr)
                          )
                          | errors
                        ]}}

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
             {:ok, acc_env,
              [BeamLang.Error.new(:type, "Non-void return requires a value.", span) | errors]}}
          end

        {:return, %{expr: expr}} ->
          case type_of_expr(expr, func_table, type_table, acc_env, return_type) do
            {:error, :unknown_function} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(:type, "Unknown function in expression.", expr_span(expr))
                  | errors
                ]}}

            {:error, :wrong_arity} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Function called with wrong number of arguments.",
                    expr_span(expr)
                  )
                  | errors
                ]}}

            {:error, :unknown_variable} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(:type, "Unknown variable in expression.", expr_span(expr))
                  | errors
                ]}}

            {:error, :unknown_type} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(:type, "Unknown type in expression.", expr_span(expr))
                  | errors
                ]}}

            {:error, :missing_type_annotation} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Struct literal requires a type annotation.",
                    expr_span(expr)
                  )
                  | errors
                ]}}

            {:error, :not_a_struct} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Field access requires a struct value.",
                    expr_span(expr)
                  )
                  | errors
                ]}}

            {:error, {:internal_field, field}} ->
              {:cont,
               {:ok, acc_env,
                [BeamLang.Error.new(:type, "Field '#{field}' is internal and cannot be accessed directly.", expr_span(expr)) | errors]}}

            {:error, {:unknown_field, field}} ->
              {:cont,
               {:ok, acc_env,
                [BeamLang.Error.new(:type, "Unknown field '#{field}'.", expr_span(expr)) | errors]}}

            {:error, {:struct, errs}} ->
              {:cont, {:ok, acc_env, errs ++ errors}}

            {:error, {:match, errs}} ->
              {:cont, {:ok, acc_env, errs ++ errors}}

            {:error, {:call, errs}} ->
              {:cont, {:ok, acc_env, errs ++ errors}}

            {:ok, inferred} ->
              if return_type == nil or return_type == :void or
                   type_compatible?(return_type, inferred) do
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
               {:ok, acc_env,
                [
                  BeamLang.Error.new(:type, "Unknown function in expression.", expr_span(expr))
                  | errors
                ]}}

            {:error, :wrong_arity} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Function called with wrong number of arguments.",
                    expr_span(expr)
                  )
                  | errors
                ]}}

            {:error, :unknown_variable} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(:type, "Unknown variable in expression.", expr_span(expr))
                  | errors
                ]}}

            {:error, :unknown_type} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(:type, "Unknown type in expression.", expr_span(expr))
                  | errors
                ]}}

            {:error, :internal_function} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Internal function cannot be called outside its module.",
                    expr_span(expr)
                  )
                  | errors
                ]}}

            {:error, :missing_type_annotation} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Struct literal requires a type annotation.",
                    expr_span(expr)
                  )
                  | errors
                ]}}

            {:error, :not_a_struct} ->
              {:cont,
               {:ok, acc_env,
                [
                  BeamLang.Error.new(
                    :type,
                    "Field access requires a struct value.",
                    expr_span(expr)
                  )
                  | errors
                ]}}

            {:error, {:internal_field, field}} ->
              {:cont,
               {:ok, acc_env,
                [BeamLang.Error.new(:type, "Field '#{field}' is internal and cannot be accessed directly.", expr_span(expr)) | errors]}}

            {:error, {:unknown_field, field}} ->
              {:cont,
               {:ok, acc_env,
                [BeamLang.Error.new(:type, "Unknown field '#{field}'.", expr_span(expr)) | errors]}}

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
      Enum.reduce(functions, %{}, fn {:function,
                                      %{
                                        name: name,
                                        type_params: type_params,
                                        params: params,
                                        return_type: return_type,
                                        internal: internal,
                                        span: span
                                      }},
                                     acc ->
        param_types =
          params
          |> Enum.map(&normalize_type(&1.type))
          |> Enum.map(&replace_type_params(&1, type_params))

        param_names = Enum.map(params, & &1.name)
        return_type = return_type |> normalize_type() |> replace_type_params(type_params)

        Map.put(acc, name, %{
          params: param_types,
          param_names: param_names,
          return: return_type,
          type_params: type_params,
          internal: internal,
          span: span
        })
      end)

    {:ok, table}
  end

  defp internal_call?(true, func_span, expr_span) do
    func_span.file_id != expr_span.file_id
  end

  defp internal_call?(_internal, _func_span, _expr_span), do: false

  @spec build_type_table([BeamLang.AST.type_def()]) :: {:ok, map()}
  defp build_type_table(types) do
    table =
      types
      |> Enum.reduce(%{}, fn {:type_def, %{name: name, params: params, fields: fields} = type_def}, acc ->
        field_order = Enum.map(fields, & &1.name)
        operators = Map.get(type_def, :operators, [])

        field_map =
          Enum.reduce(fields, %{}, fn %{name: fname, type: ftype} = field, f_acc ->
            internal = Map.get(field, :internal, false)
            Map.put(f_acc, fname, %{type: normalize_type(ftype), internal: internal})
          end)

        operator_map =
          Enum.reduce(operators, %{}, fn %{op: op, func: func_name}, op_acc ->
            existing = Map.get(op_acc, op, [])
            Map.put(op_acc, op, [func_name | existing])
          end)

        Map.put(acc, name, %{params: params, fields: field_map, field_order: field_order, operators: operator_map})
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

  @spec validate_struct_fields(
          [BeamLang.AST.field_assign()],
          map(),
          map(),
          map(),
          map(),
          binary()
        ) ::
          [BeamLang.Error.t()]
  defp validate_struct_fields(fields, type_fields, func_table, type_table, env, type_name) do
    provided = Map.new(fields, fn %{name: name} -> {name, true} end)

    missing =
      type_fields
      |> Map.keys()
      |> Enum.reject(&Map.has_key?(provided, &1))

    missing_errors =
      Enum.map(missing, fn field ->
        BeamLang.Error.new(
          :type,
          "Missing field '#{field}' for #{type_name}.",
          BeamLang.Span.new("<source>", 0, 0)
        )
      end)

    field_errors =
      Enum.flat_map(fields, fn %{name: name, expr: expr, span: span} ->
        case Map.fetch(type_fields, name) do
          :error ->
            [BeamLang.Error.new(:type, "Unknown field '#{name}' in struct literal.", span)]

          {:ok, %{type: field_type}} ->
            method_errors =
              case {field_type, expr} do
                {{:fn, [_ | _], _return_type}, {:identifier, %{name: func_name, span: expr_span}}} ->
                  case Map.fetch(func_table, func_name) do
                    {:ok, %{param_names: [first | _]}} ->
                      if first == "self" do
                        []
                      else
                        [
                          BeamLang.Error.new(
                            :type,
                            "Method function must have first parameter named self.",
                            expr_span
                          )
                        ]
                      end

                    {:ok, %{param_names: []}} ->
                      [
                        BeamLang.Error.new(
                          :type,
                          "Method function must take self as first parameter.",
                          expr_span
                        )
                      ]

                    _ ->
                      []
                  end

                {{:fn, [_ | _], _return_type}, {:lambda, %{params: params, span: expr_span}}} ->
                  case params do
                    [%{name: "self"} | _] ->
                      []

                    [] ->
                      [
                        BeamLang.Error.new(
                          :type,
                          "Method function must take self as first parameter.",
                          expr_span
                        )
                      ]

                    _ ->
                      [
                        BeamLang.Error.new(
                          :type,
                          "Method function must have first parameter named self.",
                          expr_span
                        )
                      ]
                  end

                _ ->
                  []
              end

            case type_of_expr(expr, func_table, type_table, env, field_type) do
              {:ok, inferred} ->
                if type_compatible?(field_type, inferred) do
                  method_errors
                else
                  [
                    BeamLang.Error.new(
                      :type,
                      "Field '#{name}' expected #{type_label(field_type)}, got #{type_label(inferred)}.",
                      expr_span(expr)
                    )
                    | method_errors
                  ]
                end

              {:error, :unknown_variable} ->
                [
                  BeamLang.Error.new(
                    :type,
                    "Unknown variable in field '#{name}'.",
                    expr_span(expr)
                  )
                  | method_errors
                ]

              {:error, :unknown_function} ->
                [
                  BeamLang.Error.new(
                    :type,
                    "Unknown function in field '#{name}'.",
                    expr_span(expr)
                  )
                  | method_errors
                ]

              {:error, :internal_function} ->
                [
                  BeamLang.Error.new(
                    :type,
                    "Internal function cannot be called outside its module.",
                    expr_span(expr)
                  )
                  | method_errors
                ]

              {:error, _} ->
                [
                  BeamLang.Error.new(:type, "Invalid value for field '#{name}'.", expr_span(expr))
                  | method_errors
                ]
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

    # Find return statement anywhere in the block (not just last)
    type = find_block_return_type(stmts, func_table, type_table, env)

    {type, stmt_errors}
  end

  # Find the type from a return statement in the block
  defp find_block_return_type(stmts, func_table, type_table, env) do
    # First check for explicit return statements
    return_type = Enum.find_value(stmts, fn
      {:return, %{expr: nil}} -> :void
      {:return, %{expr: expr}} -> type_or_unknown(expr, func_table, type_table, env)
      _ -> nil
    end)

    if return_type do
      return_type
    else
      # Fall back to last statement type
      case List.last(stmts) do
        {:expr, %{expr: expr}} -> type_or_unknown(expr, func_table, type_table, env)
        _ -> :void
      end
    end
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

  defp pattern_bindings({:pat_identifier, %{name: name, span: span}}, match_type, _type_table) do
    if name == "self" do
      {%{}, [BeamLang.Error.new(:type, "self is reserved for method receivers.", span)]}
    else
      {%{name => %{type: match_type, mutable: false}}, []}
    end
  end

  defp pattern_bindings({:opt_some_pat, %{name: name, span: span}}, match_type, _type_table) do
    case match_type do
      {:optional, inner} ->
        if name == "_" do
          {%{}, []}
        else
          if name == "self" do
            {%{}, [BeamLang.Error.new(:type, "self is reserved for method receivers.", span)]}
          else
            {%{name => %{type: inner, mutable: false}}, []}
          end
        end

      _ ->
        {%{}, [BeamLang.Error.new(:type, "Optional pattern requires an optional value.", span)]}
    end
  end

  defp pattern_bindings({:opt_none_pat, %{span: span}}, match_type, _type_table) do
    case match_type do
      {:optional, _inner} ->
        {%{}, []}

      _ ->
        {%{}, [BeamLang.Error.new(:type, "Optional pattern requires an optional value.", span)]}
    end
  end

  defp pattern_bindings({:res_ok_pat, %{name: name, span: span}}, match_type, _type_table) do
    case match_type do
      {:result, ok_type, _err_type} ->
        if name == "_" do
          {%{}, []}
        else
          if name == "self" do
            {%{}, [BeamLang.Error.new(:type, "self is reserved for method receivers.", span)]}
          else
            {%{name => %{type: ok_type, mutable: false}}, []}
          end
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
          if name == "self" do
            {%{}, [BeamLang.Error.new(:type, "self is reserved for method receivers.", span)]}
          else
            {%{name => %{type: err_type, mutable: false}}, []}
          end
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

  defp pattern_bindings(
         {:struct_pattern, %{name: name, fields: fields, span: span}},
         match_type,
         type_table
       ) do
    case struct_type_info(match_type) do
      {:ok, ^name, args} ->
        case Map.fetch(type_table, name) do
          {:ok, %{params: params, fields: field_map}} ->
            case type_arg_map(params, args) do
              {:ok, param_map} ->
                type_fields = substitute_field_types(field_map, param_map)

                Enum.reduce(fields, {%{}, []}, fn %{
                                                    name: field_name,
                                                    pattern: pattern,
                                                    span: field_span
                                                  },
                                                  {env, errors} ->
                  case Map.fetch(type_fields, field_name) do
                    :error ->
                      {env,
                       [
                         BeamLang.Error.new(
                           :type,
                           "Unknown field '#{field_name}' in struct pattern.",
                           field_span
                         )
                         | errors
                       ]}

                    {:ok, %{type: field_type}} ->
                      {bindings, field_errors} = pattern_bindings(pattern, field_type, type_table)
                      {Map.merge(env, bindings), errors ++ field_errors}
                  end
                end)

              :error ->
                {%{},
                 [BeamLang.Error.new(:type, "Unknown type '#{name}' in struct pattern.", span)]}
            end

          :error ->
            {%{}, [BeamLang.Error.new(:type, "Unknown type '#{name}' in struct pattern.", span)]}
        end

      {:ok, other, _args} ->
        {%{},
         [
           BeamLang.Error.new(
             :type,
             "Struct pattern expects #{name}, got #{type_label({:named, other})}.",
             span
           )
         ]}

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

      :internal_function ->
        [
          BeamLang.Error.new(
            :type,
            "Internal function cannot be called outside its module.",
            expr_span(expr)
          )
        ]

      :missing_type_annotation ->
        [BeamLang.Error.new(:type, "Struct literal requires a type annotation.", expr_span(expr))]

      :missing_optional_context ->
        [
          BeamLang.Error.new(
            :type,
            "Optional literal requires an optional type context.",
            expr_span(expr)
          )
        ]

      :missing_result_context ->
        [
          BeamLang.Error.new(
            :type,
            "Result literal requires a result type context.",
            expr_span(expr)
          )
        ]

      :not_a_struct ->
        [BeamLang.Error.new(:type, "Field access requires a struct value.", expr_span(expr))]

      {:internal_field, field} ->
        [BeamLang.Error.new(:type, "Field '#{field}' is internal and cannot be accessed directly.", expr_span(expr))]

      {:unknown_field, field} ->
        [BeamLang.Error.new(:type, "Unknown field '#{field}'.", expr_span(expr))]

      {:struct, errs} ->
        errs

      {:match, errs} ->
        errs

      {:call, errs} ->
        errs

      :wrong_arity ->
        [
          BeamLang.Error.new(
            :type,
            "Function called with wrong number of arguments.",
            expr_span(expr)
          )
        ]

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
      [
        BeamLang.Error.new(
          :type,
          "Match guard only supports comparison operators.",
          expr_span(expr)
        )
      ]
    end
  end

  defp validate_param_names(params) do
    params
    |> Enum.with_index()
    |> Enum.flat_map(fn {%{name: name, span: span}, idx} ->
      if name == "self" and idx != 0 do
        [BeamLang.Error.new(:type, "self must be the first parameter.", span)]
      else
        []
      end
    end)
  end

  # Add method type context to env if first param is 'self'
  # This allows internal field access for any variable of the same type within method functions
  defp add_method_type_context(env, [%{name: "self", type: self_type} | _]) do
    case base_type_name(self_type) do
      {:ok, type_name} -> Map.put(env, :__method_type__, type_name)
      :error -> env
    end
  end

  defp add_method_type_context(env, _params), do: env

  # Extract base type name from a type (handles generics like List<T>)
  defp base_type_name({:named, name}), do: {:ok, name}
  defp base_type_name({:generic, {:named, name}, _}), do: {:ok, name}
  defp base_type_name(_), do: :error

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

  @spec else_errors(
          BeamLang.AST.if_else_branch() | nil,
          map(),
          map(),
          map(),
          non_neg_integer(),
          BeamLang.AST.type_name() | nil,
          [BeamLang.Error.t()]
        ) ::
          {[BeamLang.Error.t()], map()}
  defp else_errors(nil, _func_table, _type_table, _env, _loop_depth, _return_type, errors),
    do: {errors, %{}}

  defp else_errors(
         {:else_block, %{block: block}},
         func_table,
         type_table,
         env,
         loop_depth,
         return_type,
         errors
       ) do
    {:ok, _env_else, else_errors} =
      validate_statements(block, func_table, type_table, env, loop_depth, return_type)

    {else_errors ++ errors, %{}}
  end

  defp else_errors(
         {:else_if, %{if: if_stmt}},
         func_table,
         type_table,
         env,
         loop_depth,
         return_type,
         errors
       ) do
    {:ok, _env, if_errors} =
      validate_statements(
        {:block, %{stmts: [if_stmt], span: BeamLang.Span.new("<source>", 0, 0)}},
        func_table,
        type_table,
        env,
        loop_depth,
        return_type
      )

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

  @spec iterator_errors(BeamLang.AST.expr(), map(), map(), map(), BeamLang.Span.t()) :: [
          BeamLang.Error.t()
        ]
  defp iterator_errors(collection, func_table, type_table, env, span) do
    case type_of_expr(collection, func_table, type_table, env, nil) do
      {:ok, type} ->
        case iterator_item_type_from_type(type) do
          {:ok, _item_type} ->
            []

          :error ->
            [
              BeamLang.Error.new(
                :type,
                "For loop requires Iterator<T>, got #{type_label(type)}.",
                span
              )
            ]
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
      {:generic, {:named, "Iterator"}, [item_type]} -> {:ok, item_type}
      {:generic, {:named, "List"}, [item_type]} -> {:ok, item_type}
      :range -> {:ok, :number}
      _ -> :error
    end
  end

  @spec expr_span(BeamLang.AST.expr()) :: BeamLang.Span.t()
  defp expr_span({:integer, %{span: span}}), do: span
  defp expr_span({:float, %{span: span}}), do: span
  defp expr_span({:string, %{span: span}}), do: span
  defp expr_span({:interpolated_string, %{span: span}}), do: span
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
  defp expr_span({:lambda, %{span: span}}), do: span
  defp expr_span({:method_call, %{span: span}}), do: span
  defp expr_span({:list_literal, %{span: span}}), do: span
  defp expr_span({:range, %{span: span}}), do: span

  @spec type_compatible?(BeamLang.AST.type_name(), BeamLang.AST.type_name()) :: boolean()
  defp type_compatible?(expected, inferred) do
    expected = normalize_type(expected)
    inferred = normalize_type(inferred)
    do_type_compatible?(expected, inferred)
  end

  defp do_type_compatible?(:any, _inferred), do: true
  defp do_type_compatible?(_expected, :any), do: true
  defp do_type_compatible?({:type_var, _name}, _inferred), do: true
  defp do_type_compatible?(_expected, {:type_var, _name}), do: true
  defp do_type_compatible?(expected, inferred) when expected == inferred, do: true

  defp do_type_compatible?({:optional, expected_inner}, {:optional, inferred_inner}),
    do: type_compatible?(expected_inner, inferred_inner)

  defp do_type_compatible?(
         {:result, expected_ok, expected_err},
         {:result, inferred_ok, inferred_err}
       ),
       do:
         type_compatible?(expected_ok, inferred_ok) and
           type_compatible?(expected_err, inferred_err)

  defp do_type_compatible?(
         {:generic, expected_base, expected_args},
         {:generic, inferred_base, inferred_args}
       ) do
    length(expected_args) == length(inferred_args) and
      type_compatible?(expected_base, inferred_base) and
      Enum.zip(expected_args, inferred_args)
      |> Enum.all?(fn {exp, inf} -> type_compatible?(exp, inf) end)
  end

  defp do_type_compatible?(
         {:fn, expected_params, expected_ret},
         {:fn, inferred_params, inferred_ret}
       ) do
    length(expected_params) == length(inferred_params) and
      Enum.zip(expected_params, inferred_params)
      |> Enum.all?(fn {exp, inf} -> type_compatible?(exp, inf) end) and
      type_compatible?(expected_ret, inferred_ret)
  end

  defp do_type_compatible?(_expected, _inferred), do: false

  @spec comparable_types?(BeamLang.AST.type_name(), BeamLang.AST.type_name()) :: boolean()
  defp comparable_types?(left, right) do
    left = normalize_type(left)
    right = normalize_type(right)
    do_comparable_types?(left, right)
  end

  defp do_comparable_types?(left, right) when left == right, do: true
  defp do_comparable_types?(_left, _right), do: false

  @spec types_match?(BeamLang.AST.type_name(), BeamLang.AST.type_name()) :: boolean()
  defp types_match?(type1, type2) do
    normalized1 = normalize_type(type1)
    normalized2 = normalize_type(type2)
    do_types_match?(normalized1, normalized2)
  end

  defp do_types_match?(same, same), do: true
  defp do_types_match?({:named, name}, {:named, name}), do: true
  defp do_types_match?({:generic, base1, args1}, {:generic, base2, args2}) do
    do_types_match?(base1, base2) and length(args1) == length(args2) and
      Enum.zip(args1, args2) |> Enum.all?(fn {a, b} -> do_types_match?(a, b) end)
  end
  defp do_types_match?(_type1, _type2), do: false

  @spec arithmetic_type(atom(), BeamLang.AST.type_name(), BeamLang.AST.type_name()) ::
          {:ok, BeamLang.AST.type_name()} | {:error, :invalid_binary_op}
  defp arithmetic_type(op, left, right) do
    cond do
      left == :number and right == :number and op in [:add, :sub, :mul, :div, :mod] ->
        {:ok, :number}

      left == :String and right == :String and op == :add ->
        {:ok, :String}

      true ->
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
          {:ok,
           %{
             name: binary(),
             type: BeamLang.AST.type_name(),
             mutable: boolean(),
             field: binary() | nil
           }}
          | {:error, BeamLang.Error.t()}
  defp assignment_target({:identifier, %{name: name, span: span}}, env) do
    if name == "self" do
      {:error, BeamLang.Error.new(:type, "self is reserved for method receivers.", span)}
    else
      case Map.fetch(env, name) do
        {:ok, %{type: type, mutable: mutable}} ->
          {:ok, %{name: name, type: type, mutable: mutable, field: nil}}

        :error ->
          {:error, BeamLang.Error.new(:type, "Unknown variable in assignment.", span)}
      end
    end
  end

  defp assignment_target({:field, %{target: target, name: field, span: span}}, env) do
    case target do
      {:identifier, %{name: name}} ->
        case Map.fetch(env, name) do
          {:ok, %{type: type, mutable: mutable}} ->
            {:ok, %{name: name, type: type, mutable: mutable, field: field}}

          :error ->
            {:error, BeamLang.Error.new(:type, "Unknown variable in assignment.", span)}
        end

      _ ->
        {:error,
         BeamLang.Error.new(:type, "Assignment target must be a variable or struct field.", span)}
    end
  end

  defp assignment_target(_other, _env) do
    {:error,
     BeamLang.Error.new(:type, "Invalid assignment target.", BeamLang.Span.new("<source>", 0, 0))}
  end

  @spec resolve_field_type(BeamLang.AST.type_name(), binary(), map(), boolean(), binary() | nil) ::
          {:ok, BeamLang.AST.type_name()} | {:error, BeamLang.Error.t()}
  defp resolve_field_type(type, field, type_table, is_self_access, method_type) do
    case struct_type_info(type) do
      {:ok, type_name, args} ->
        # Allow internal field access if accessing via self OR within a method of the same type
        is_same_type_method = method_type != nil and method_type == type_name
        can_access_internal = is_self_access or is_same_type_method

        case Map.fetch(type_table, type_name) do
          {:ok, %{params: params, fields: field_map}} ->
            case type_arg_map(params, args) do
              {:ok, param_map} ->
                type_fields = substitute_field_types(field_map, param_map)

                case Map.fetch(type_fields, field) do
                  {:ok, %{type: _field_type, internal: true}} when not can_access_internal ->
                    {:error,
                     BeamLang.Error.new(
                       :type,
                       "Field '#{field}' is internal and cannot be accessed directly.",
                       BeamLang.Span.new("<source>", 0, 0)
                     )}

                  {:ok, %{type: field_type, internal: _}} ->
                    {:ok, field_type}

                  :error ->
                    {:error,
                     BeamLang.Error.new(
                       :type,
                       "Unknown field '#{field}'.",
                       BeamLang.Span.new("<source>", 0, 0)
                     )}
                end

              :error ->
                {:error,
                 BeamLang.Error.new(
                   :type,
                   "Unknown type '#{type_name}'.",
                   BeamLang.Span.new("<source>", 0, 0)
                 )}
            end

          :error ->
            {:error,
             BeamLang.Error.new(
               :type,
               "Unknown type '#{type_name}'.",
               BeamLang.Span.new("<source>", 0, 0)
             )}
        end

      :error ->
        {:error,
         BeamLang.Error.new(
           :type,
           "Field access requires a struct value.",
           BeamLang.Span.new("<source>", 0, 0)
         )}
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

  @spec select_match_type([{BeamLang.AST.type_name(), BeamLang.Span.t()}]) ::
          BeamLang.AST.type_name()
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
  defp struct_type_info(:String), do: {:ok, "String", []}
  defp struct_type_info({:optional, inner}), do: {:ok, "Optional", [inner]}
  defp struct_type_info({:result, ok_type, err_type}), do: {:ok, "Result", [ok_type, err_type]}
  defp struct_type_info({:named, name}) when is_binary(name), do: {:ok, name, []}
  defp struct_type_info({:generic, {:named, name}, args}), do: {:ok, name, args}
  defp struct_type_info(_type), do: :error

  defp annotate_program(
         {:program,
          %{module: module, imports: imports, types: types, functions: functions, span: span} = prog},
         func_table,
         type_table
       ) do
    functions = Enum.map(functions, &annotate_function(&1, func_table, type_table))
    errors = Map.get(prog, :errors, [])

    {:program,
     %{module: module, imports: imports, types: types, errors: errors, functions: functions, span: span}}
  end

  defp annotate_function({:function, %{body: nil}} = func, _func_table, _type_table), do: func

  defp annotate_function(
         {:function,
          %{params: params, return_type: return_type, type_params: type_params, body: body} = info},
         func_table,
         type_table
       ) do
    env =
      Enum.reduce(params, %{}, fn %{name: name, type: type}, acc ->
        type = type |> normalize_type() |> replace_type_params(type_params)
        Map.put(acc, name, %{type: type, mutable: false})
      end)

    return_type = return_type |> normalize_type() |> replace_type_params(type_params)
    {body, _env} = annotate_block(body, env, func_table, type_table, return_type)
    {:function, %{info | body: body}}
  end

  defp annotate_block(
         {:block, %{stmts: stmts, span: span}},
         env,
         func_table,
         type_table,
         return_type
       ) do
    {stmts, env} =
      Enum.map_reduce(stmts, env, fn stmt, acc_env ->
        annotate_stmt(stmt, acc_env, func_table, type_table, return_type)
      end)

    {{:block, %{stmts: stmts, span: span}}, env}
  end

  defp annotate_stmt(
         {:let, %{name: name, expr: expr, mutable: mutable, type: declared_type} = info},
         env,
         func_table,
         type_table,
         _return_type
       ) do
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
    info = Map.put(info, :expr, expr)
    info = Map.put(info, :inferred_type, inferred)
    {{:let, info}, env}
  end

  defp annotate_stmt(
         {:assign, %{target: target, expr: expr} = info},
         env,
         func_table,
         type_table,
         _return_type
       ) do
    expected =
      case assignment_target(target, env) do
        {:ok, %{type: type}} -> type
        _ -> nil
      end

    {expr, _inferred} = annotate_expr(expr, expected, env, func_table, type_table)
    {{:assign, %{info | expr: expr}}, env}
  end

  defp annotate_stmt({:return, %{expr: nil}} = stmt, env, _func_table, _type_table, _return_type),
    do: {stmt, env}

  defp annotate_stmt({:return, %{expr: expr} = info}, env, func_table, type_table, return_type) do
    {expr, _inferred} = annotate_expr(expr, return_type, env, func_table, type_table)
    {{:return, %{info | expr: expr}}, env}
  end

  defp annotate_stmt({:expr, %{expr: expr} = info}, env, func_table, type_table, _return_type) do
    {expr, _inferred} = annotate_expr(expr, nil, env, func_table, type_table)
    {{:expr, %{info | expr: expr}}, env}
  end

  defp annotate_stmt(
         {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch} = info},
         env,
         func_table,
         type_table,
         return_type
       ) do
    {cond, _} = annotate_expr(cond, :bool, env, func_table, type_table)
    {then_block, _} = annotate_block(then_block, env, func_table, type_table, return_type)
    else_branch = annotate_else_branch(else_branch, env, func_table, type_table, return_type)
    {{:if_stmt, %{info | cond: cond, then_block: then_block, else_branch: else_branch}}, env}
  end

  defp annotate_stmt(
         {:while, %{cond: cond, body: body} = info},
         env,
         func_table,
         type_table,
         return_type
       ) do
    {cond, _} = annotate_expr(cond, :bool, env, func_table, type_table)
    {body, _} = annotate_block(body, env, func_table, type_table, return_type)
    {{:while, %{info | cond: cond, body: body}}, env}
  end

  defp annotate_stmt({:loop, %{body: body} = info}, env, func_table, type_table, return_type) do
    {body, _} = annotate_block(body, env, func_table, type_table, return_type)
    {{:loop, %{info | body: body}}, env}
  end

  defp annotate_stmt(
         {:for, %{name: name, collection: collection, body: body} = info},
         env,
         func_table,
         type_table,
         return_type
       ) do
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

    updated =
      Map.merge(info, %{
        collection: collection,
        body: body,
        collection_type: collection_type,
        item_type: item_type
      })

    {{:for, updated}, env}
  end

  defp annotate_stmt(
         {:guard, %{cond: cond, else_block: else_block} = info},
         env,
         func_table,
         type_table,
         return_type
       ) do
    {cond, _} = annotate_expr(cond, :bool, env, func_table, type_table)
    {else_block, _} = annotate_block(else_block, env, func_table, type_table, return_type)
    {{:guard, %{info | cond: cond, else_block: else_block}}, env}
  end

  defp annotate_stmt(stmt, env, _func_table, _type_table, _return_type), do: {stmt, env}

  defp annotate_else_branch(nil, _env, _func_table, _type_table, _return_type), do: nil

  defp annotate_else_branch(
         {:else_block, %{block: block, span: span}},
         env,
         func_table,
         type_table,
         return_type
       ) do
    {block, _} = annotate_block(block, env, func_table, type_table, return_type)
    {:else_block, %{block: block, span: span}}
  end

  defp annotate_else_branch(
         {:else_if, %{if: if_stmt, span: span}},
         env,
         func_table,
         type_table,
         return_type
       ) do
    {if_stmt, _} = annotate_stmt(if_stmt, env, func_table, type_table, return_type)
    {:else_if, %{if: if_stmt, span: span}}
  end

  defp annotate_expr(
         {:struct, %{fields: fields, span: span} = info},
         expected,
         env,
         func_table,
         type_table
       ) do
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

  defp annotate_expr(
         {:call, %{name: name, args: args, span: span} = info},
         _expected,
         env,
         func_table,
         type_table
       ) do
    type_args = Map.get(info, :type_args, [])

    args =
      case Map.fetch(func_table, name) do
        {:ok, %{params: param_types}} ->
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

    type_info =
      if name == "parse_args" do
        case parse_args_type_info(type_args, type_table, span) do
          {:ok, info} -> info
          {:error, _} -> nil
        end
      else
        nil
      end

    {{:call, %{name: name, args: args, span: span, type_args: type_args, type_info: type_info}}, nil}
  end

  defp annotate_expr(
         {:field, %{target: target, name: name, span: span}},
         _expected,
         env,
         func_table,
         type_table
       ) do
    {target, _} = annotate_expr(target, nil, env, func_table, type_table)
    {{:field, %{target: target, name: name, span: span}}, nil}
  end

  defp annotate_expr(
         {:match, %{expr: expr, cases: cases, span: span}},
         expected,
         env,
         func_table,
         type_table
       ) do
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

  defp annotate_expr(
         {:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch, span: span}},
         expected,
         env,
         func_table,
         type_table
       ) do
    {cond, _} = annotate_expr(cond, :bool, env, func_table, type_table)
    {then_block, _} = annotate_block(then_block, env, func_table, type_table, expected)
    else_branch = annotate_else_branch(else_branch, env, func_table, type_table, expected)

    {{:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch, span: span}},
     expected}
  end

  defp annotate_expr(
         {:block_expr, %{block: block, span: span}},
         expected,
         env,
         func_table,
         type_table
       ) do
    {block, _} = annotate_block(block, env, func_table, type_table, expected)
    {{:block_expr, %{block: block, span: span}}, expected}
  end

  defp annotate_expr(
         {:binary, %{op: op, left: left, right: right, span: span}},
         _expected,
         env,
         func_table,
         type_table
       ) do
    {left, left_type} = annotate_expr(left, nil, env, func_table, type_table)
    {right, right_type} = annotate_expr(right, nil, env, func_table, type_table)

    # Determine the inferred left type
    inferred_left_type =
      case left_type do
        nil ->
          case type_of_expr(left, func_table, type_table, env, nil) do
            {:ok, t} -> t
            _ -> nil
          end
        t -> t
      end

    inferred_right_type =
      case right_type do
        nil ->
          case type_of_expr(right, func_table, type_table, env, nil) do
            {:ok, t} -> t
            _ -> nil
          end
        t -> t
      end

    # Check if operator is overloaded
    operator_info =
      if inferred_left_type != nil do
        case check_operator_overload(op, inferred_left_type, inferred_right_type, type_table, func_table) do
          {:ok, result_type, func_name} ->
            %{overloaded: true, left_type: inferred_left_type, right_type: inferred_right_type, result_type: result_type, func_name: func_name}
          :no_overload ->
            nil
        end
      else
        nil
      end

    {{:binary, %{op: op, left: left, right: right, span: span, operator_info: operator_info}}, nil}
  end

  defp annotate_expr(
         {:opt_some, %{expr: expr, span: span}},
         expected,
         env,
         func_table,
         type_table
       ) do
    inner =
      case normalize_type(expected) do
        {:optional, inner} -> inner
        _ -> nil
      end

    {expr, _} = annotate_expr(expr, inner, env, func_table, type_table)

    inferred =
      case type_of_expr(
             {:opt_some, %{expr: expr, span: span}},
             func_table,
             type_table,
             env,
             expected
           ) do
        {:ok, type} -> type
        _ -> normalize_type(expected)
      end

    {{:opt_some, %{expr: expr, span: span, type: inferred}}, inferred}
  end

  defp annotate_expr({:opt_none, %{span: span}}, expected, env, func_table, type_table) do
    inferred =
      case type_of_expr({:opt_none, %{span: span}}, func_table, type_table, env, expected) do
        {:ok, type} -> type
        _ -> normalize_type(expected)
      end

    {{:opt_none, %{span: span, type: inferred}}, inferred}
  end

  defp annotate_expr({:res_ok, %{expr: expr, span: span}}, expected, env, func_table, type_table) do
    inner =
      case normalize_type(expected) do
        {:result, ok_type, _} -> ok_type
        _ -> nil
      end

    {expr, _} = annotate_expr(expr, inner, env, func_table, type_table)

    inferred =
      case type_of_expr(
             {:res_ok, %{expr: expr, span: span}},
             func_table,
             type_table,
             env,
             expected
           ) do
        {:ok, type} -> type
        _ -> normalize_type(expected)
      end

    {{:res_ok, %{expr: expr, span: span, type: inferred}}, inferred}
  end

  defp annotate_expr({:res_err, %{expr: expr, span: span}}, expected, env, func_table, type_table) do
    inner =
      case normalize_type(expected) do
        {:result, _, err_type} -> err_type
        _ -> nil
      end

    {expr, _} = annotate_expr(expr, inner, env, func_table, type_table)

    inferred =
      case type_of_expr(
             {:res_err, %{expr: expr, span: span}},
             func_table,
             type_table,
             env,
             expected
           ) do
        {:ok, type} -> type
        _ -> normalize_type(expected)
      end

    {{:res_err, %{expr: expr, span: span, type: inferred}}, inferred}
  end

  defp annotate_expr({:list_literal, %{elements: elements, span: span}}, expected, env, func_table, type_table) do
    elem_type =
      case normalize_type(expected) do
        {:generic, {:named, "List"}, [t]} -> t
        _ -> nil
      end

    annotated_elements =
      Enum.map(elements, fn elem ->
        {annotated, _} = annotate_expr(elem, elem_type, env, func_table, type_table)
        annotated
      end)

    inferred =
      case type_of_expr(
             {:list_literal, %{elements: elements, span: span}},
             func_table,
             type_table,
             env,
             expected
           ) do
        {:ok, type} -> type
        _ -> {:generic, {:named, "List"}, [:any]}
      end

    {{:list_literal, %{elements: annotated_elements, span: span, type: inferred}}, inferred}
  end

  defp annotate_expr(
         {:method_call, %{target: target, name: name, args: args, span: span}},
         _expected,
         env,
         func_table,
         type_table
       ) do
    {target, _} = annotate_expr(target, nil, env, func_table, type_table)
    {args, _} = annotate_expr_list(args, env, func_table, type_table)

    target_type =
      case type_of_expr(target, func_table, type_table, env, nil) do
        {:ok, type} -> normalize_type(type)
        _ -> :any
      end

    {{:method_call,
      %{target: target, name: name, args: args, span: span, target_type: target_type}}, nil}
  end

  defp annotate_expr(expr, _expected, _env, _func_table, _type_table), do: {expr, nil}

  defp annotate_expr_list(args, env, func_table, type_table) do
    args =
      Enum.map(args, fn arg ->
        {arg, _} = annotate_expr(arg, nil, env, func_table, type_table)
        arg
      end)

    {args, env}
  end

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

  defp normalize_type({:fn, params, return_type}),
    do: {:fn, Enum.map(params, &normalize_type/1), normalize_type(return_type)}

  defp normalize_type({:type_var, name}), do: {:type_var, name}

  defp normalize_type(:any), do: :any

  defp normalize_type(type), do: type

  defp replace_type_params(type, type_params) do
    case type do
      {:named, name} ->
        if Enum.member?(type_params, name) do
          {:type_var, name}
        else
          {:named, name}
        end

      {:generic, base, args} ->
        {:generic, replace_type_params(base, type_params),
         Enum.map(args, &replace_type_params(&1, type_params))}

      {:optional, inner} ->
        {:optional, replace_type_params(inner, type_params)}

      {:result, ok_type, err_type} ->
        {:result, replace_type_params(ok_type, type_params),
         replace_type_params(err_type, type_params)}

      {:fn, params, return_type} ->
        {:fn, Enum.map(params, &replace_type_params(&1, type_params)),
         replace_type_params(return_type, type_params)}

      _ ->
        type
    end
  end

  defp infer_type_params(param_types, args, func_table, type_table, env) do
    Enum.reduce_while(Enum.zip(param_types, args), {:ok, %{}}, fn {param_type, arg},
                                                                  {:ok, mapping} ->
      case type_of_expr(arg, func_table, type_table, env, nil) do
        {:ok, inferred} ->
          case unify_types(param_type, inferred, mapping) do
            {:ok, next} -> {:cont, {:ok, next}}
            {:error, reason} -> {:halt, {:error, [type_param_error(reason, arg)]}}
          end

        {:error, reason} ->
          {:halt, {:error, expr_error(reason, arg)}}
      end
    end)
  end

  defp type_param_error({:type_mismatch, expected, inferred}, arg) do
    BeamLang.Error.new(
      :type,
      "Argument type mismatch. Expected #{type_label(expected)}, got #{type_label(inferred)}.",
      expr_span(arg)
    )
  end

  defp type_param_error({:cannot_infer, name}, arg) do
    BeamLang.Error.new(:type, "Cannot infer type parameter #{name}.", expr_span(arg))
  end

  defp unify_types(expected, inferred, mapping) do
    expected = normalize_type(expected)
    inferred = normalize_type(inferred)

    # any is compatible with everything - allow type inference to proceed
    cond do
      expected == :any ->
        {:ok, mapping}

      inferred == :any ->
        {:ok, mapping}

      true ->
        do_unify_types(expected, inferred, mapping)
    end
  end

  defp do_unify_types({:type_var, name}, inferred, mapping) do
    case Map.fetch(mapping, name) do
      :error ->
        {:ok, Map.put(mapping, name, inferred)}

      {:ok, bound} ->
        if type_compatible?(bound, inferred) do
          {:ok, mapping}
        else
          {:error, {:type_mismatch, bound, inferred}}
        end
    end
  end

  defp do_unify_types({:optional, exp_inner}, {:optional, inf_inner}, mapping) do
    unify_types(exp_inner, inf_inner, mapping)
  end

  defp do_unify_types({:optional, _} = expected, inferred, _mapping) do
    {:error, {:type_mismatch, expected, inferred}}
  end

  defp do_unify_types({:result, exp_ok, exp_err}, {:result, inf_ok, inf_err}, mapping) do
    with {:ok, mapping} <- unify_types(exp_ok, inf_ok, mapping),
         {:ok, mapping} <- unify_types(exp_err, inf_err, mapping) do
      {:ok, mapping}
    end
  end

  defp do_unify_types({:result, _, _} = expected, inferred, _mapping) do
    {:error, {:type_mismatch, expected, inferred}}
  end

  defp do_unify_types({:generic, exp_base, exp_args}, {:generic, inf_base, inf_args}, mapping)
       when length(exp_args) == length(inf_args) do
    with {:ok, mapping} <- unify_types(exp_base, inf_base, mapping) do
      Enum.zip(exp_args, inf_args)
      |> Enum.reduce_while({:ok, mapping}, fn {exp_arg, inf_arg}, {:ok, acc} ->
        case unify_types(exp_arg, inf_arg, acc) do
          {:ok, next} -> {:cont, {:ok, next}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)
    end
  end

  defp do_unify_types({:generic, _, _} = expected, inferred, _mapping) do
    {:error, {:type_mismatch, expected, inferred}}
  end

  defp do_unify_types({:fn, exp_params, exp_ret}, {:fn, inf_params, inf_ret}, mapping)
       when length(exp_params) == length(inf_params) do
    with {:ok, mapping} <-
           Enum.zip(exp_params, inf_params)
           |> Enum.reduce_while({:ok, mapping}, fn {exp_param, inf_param}, {:ok, acc} ->
             case unify_types(exp_param, inf_param, acc) do
               {:ok, next} -> {:cont, {:ok, next}}
               {:error, reason} -> {:halt, {:error, reason}}
             end
           end) do
      unify_types(exp_ret, inf_ret, mapping)
    end
  end

  defp do_unify_types({:fn, _, _} = expected, inferred, _mapping) do
    {:error, {:type_mismatch, expected, inferred}}
  end

  defp do_unify_types(expected, inferred, mapping) do
    if type_compatible?(expected, inferred) do
      {:ok, mapping}
    else
      {:error, {:type_mismatch, expected, inferred}}
    end
  end

  defp substitute_type_vars(type, mapping, _span) do
    {:ok, substitute_type(type, mapping)}
  end

  defp type_arg_map(params, args) do
    if length(params) == length(args) do
      {:ok, Map.new(Enum.zip(params, args))}
    else
      :error
    end
  end

  defp substitute_field_types(field_map, param_map) do
    Enum.reduce(field_map, %{}, fn {name, %{type: type, internal: internal}}, acc ->
      Map.put(acc, name, %{type: substitute_type(type, param_map), internal: internal})
    end)
  end

  defp substitute_type({:named, name}, param_map) do
    case Map.fetch(param_map, name) do
      {:ok, type} -> type
      :error -> {:named, name}
    end
  end

  defp substitute_type({:type_var, name}, param_map) do
    case Map.fetch(param_map, name) do
      {:ok, type} -> type
      :error -> {:type_var, name}
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

  defp substitute_type({:fn, params, return_type}, param_map) do
    {:fn, Enum.map(params, &substitute_type(&1, param_map)),
     substitute_type(return_type, param_map)}
  end

  defp substitute_type(type, _param_map), do: type

  @spec type_label(BeamLang.AST.type_name()) :: binary()
  defp type_label({:generic, base, args}) do
    "#{type_label(base)}<#{Enum.map_join(args, ", ", &type_label/1)}>"
  end

  defp type_label({:type_var, name}), do: name
  defp type_label({:named, name}) when is_binary(name), do: name
  defp type_label(:any), do: "any"
  defp type_label(:number), do: "number"
  defp type_label(:char), do: "char"
  defp type_label(:integer), do: "number"
  defp type_label(:float), do: "number"
  defp type_label({:optional, inner}), do: "#{type_label(inner)}?"

  defp type_label({:result, ok_type, err_type}),
    do: "#{type_label(ok_type)}!#{type_label(err_type)}"

  defp type_label({:fn, params, return_type}) do
    "fn(#{Enum.map_join(params, ", ", &type_label/1)}) -> #{type_label(return_type)}"
  end

  defp type_label(type) when is_atom(type), do: Atom.to_string(type)
end
