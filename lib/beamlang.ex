defmodule BeamLang do
  @moduledoc """
  BeamLang compiler entry point for the MVP.
  """

  alias BeamLang.{Codegen, Lexer, Parser, Semantic, Runtime}

  @spec compile_source(binary()) ::
          {:ok,
           %{
             tokens: [BeamLang.Token.t()],
             ast: BeamLang.AST.t(),
             forms: list(),
             module: atom(),
             binary: binary()
           }}
          | {:error, [BeamLang.Error.t()]}
  def compile_source(source) when is_binary(source) do
    compile_source(source, "<source>")
  end

  @spec compile_source(binary(), binary()) ::
          {:ok,
           %{
             tokens: [BeamLang.Token.t()],
             ast: BeamLang.AST.t(),
             forms: list(),
             module: atom(),
             binary: binary()
           }}
          | {:error, [BeamLang.Error.t()]}
  def compile_source(source, filename) when is_binary(source) and is_binary(filename) do
    with {:ok, tokens} <- Lexer.tokenize(source, filename),
         {:ok, ast} <- Parser.parse(tokens),
         {:ok, stdlib_ast} <- load_stdlib_ast(),
         {:ok, stdlib_ast} <- load_ext_modules_for_imports(ast, stdlib_ast),
         {:ok, merged} <- merge_programs(stdlib_ast, ast),
         {:ok, checked} <- Semantic.validate(merged, require_main: true) do
      forms = Codegen.to_erlang_forms(checked)

      with {:ok, module, binary} <- Runtime.compile_forms(forms) do
        {:ok,
         %{
           tokens: tokens,
           ast: checked,
           forms: forms,
           module: module,
           binary: binary
         }}
      end
    else
      {:error, %BeamLang.Error{} = error} ->
        {:error, [error]}

      {:error, errors} when is_list(errors) ->
        {:error, errors}

      {:error, other} ->
        span = BeamLang.Span.new(filename, 0, 0)
        {:error, [BeamLang.Error.new(:type, inspect(other), span)]}
    end
  end

  @spec analyze_source(binary(), binary()) ::
          {:ok,
           %{
             tokens: [BeamLang.Token.t()],
             ast: BeamLang.AST.t(),
             errors: [BeamLang.Error.t()]
          }}
          | {:error, [BeamLang.Error.t()]}
  def analyze_source(source, filename) when is_binary(source) and is_binary(filename) do
    with {:ok, tokens} <- Lexer.tokenize(source, filename),
         {:ok, ast} <- Parser.parse(tokens),
         {:ok, stdlib_ast} <- load_stdlib_ast() do
      case analyze_merged_program(ast, filename, stdlib_ast) do
        {:ok, merged} ->
          case Semantic.validate(merged, require_main: false) do
            {:ok, checked} ->
              {:ok, %{tokens: tokens, ast: checked, errors: []}}

            {:error, errors} when is_list(errors) ->
              {:ok, %{tokens: tokens, ast: merged, errors: errors}}
          end

        {:error, errors} when is_list(errors) ->
          {:ok, %{tokens: tokens, ast: ast, errors: errors}}
      end
    else
      {:error, %BeamLang.Error{} = error} ->
        {:error, [error]}

      {:error, errors} when is_list(errors) ->
        {:error, errors}
    end
  end

  defp analyze_merged_program(ast, filename, stdlib_ast) do
    if File.exists?(filename) and requires_local_module_analysis?(ast) do
      analyze_merged_program_with_modules(ast, filename, stdlib_ast)
    else
      analyze_merged_program_single(ast, stdlib_ast)
    end
  end

  defp analyze_merged_program_with_modules(ast, filename, stdlib_ast) do
    module_name = module_name_from_path(filename)
    ast = set_program_module(ast, module_name)

    with {:ok, modules, _entry_module} <- load_modules(filename),
         modules <- Map.put(modules, module_name, %{ast: ast, path: filename}),
         exports <- module_exports(modules),
         {:ok, resolved} <- resolve_imports_and_qualify(ast, exports, module_name),
         {:ok, full_stdlib} <- load_ext_modules_for_imports(resolved, stdlib_ast),
         {:ok, merged} <- merge_programs(full_stdlib, resolved) do
      {:ok, merged}
    else
      {:error, errors} when is_list(errors) ->
        {:error, errors}
    end
  end

  defp analyze_merged_program_single(ast, stdlib_ast) do
    with {:ok, stdlib_ast} <- load_ext_modules_for_imports(ast, stdlib_ast),
         {:ok, merged} <- merge_programs(stdlib_ast, ast) do
      {:ok, merged}
    end
  end

  defp requires_local_module_analysis?({:program, %{imports: imports}} = program) do
    has_local_imports? =
      imports
      |> Enum.map(fn {:import, %{module: module}} -> module end)
      |> Enum.any?(fn module -> not stdlib_ext_module?(module) end)

    has_qualified_module_refs? =
      case qualified_module_refs(program) do
        [] -> false
        _ -> true
      end

    has_local_imports? or has_qualified_module_refs?
  end

  defp requires_local_module_analysis?(_), do: false

  @spec compile_file(binary()) ::
          {:ok,
           %{
             entry: atom(),
             modules: [{atom(), binary()}]
           }}
          | {:error, [BeamLang.Error.t()]}
  def compile_file(path) when is_binary(path) do
    with {:ok, modules, entry} <- load_modules(path),
         {:ok, stdlib_ast} <- load_stdlib_ast(),
         {:ok, compiled} <- compile_modules(modules, stdlib_ast, entry) do
      {:ok, %{entry: compiled.entry, modules: compiled.modules}}
    end
  end

  @stdlib_base_dir Path.expand("../stdlib", __DIR__)
  @stdlib_core_dir Path.join(@stdlib_base_dir, "core")
  @stdlib_ext_dir Path.join(@stdlib_base_dir, "ext")

  # Known ext stdlib module names (require explicit import)
  @stdlib_ext_modules ~w(system network args)

  defp load_stdlib_ast() do
    load_stdlib_dir(@stdlib_core_dir)
  end

  defp load_stdlib_dir(dir) do
    case File.ls(dir) do
      {:ok, entries} ->
        files =
          entries
          |> Enum.filter(&String.ends_with?(&1, ".bl"))
          |> Enum.sort()

        case files do
          [] ->
            span = BeamLang.Span.new(dir, 0, 0)
            {:error, [BeamLang.Error.new(:type, "No stdlib files found.", span)]}

          _ ->
            Enum.reduce_while(files, {:ok, empty_program()}, fn file, {:ok, acc} ->
              path = Path.join(dir, file)

              case File.read(path) do
                {:ok, source} ->
                  with {:ok, tokens} <- Lexer.tokenize(source, path),
                       {:ok, ast} <- Parser.parse(tokens) do
                    {:cont, {:ok, merge_programs_simple(acc, ast)}}
                  else
                    {:error, %BeamLang.Error{} = err} -> {:halt, {:error, [err]}}
                    {:error, errs} when is_list(errs) -> {:halt, {:error, errs}}
                  end

                {:error, reason} ->
                  span = BeamLang.Span.new(path, 0, 0)

                  {:halt,
                   {:error,
                    [BeamLang.Error.new(:type, "Failed to read stdlib: #{inspect(reason)}", span)]}}
              end
            end)
        end

      {:error, reason} ->
        span = BeamLang.Span.new(dir, 0, 0)

        {:error,
         [BeamLang.Error.new(:type, "Failed to read stdlib directory: #{inspect(reason)}", span)]}
    end
  end

  @doc false
  def stdlib_ext_dir, do: @stdlib_ext_dir

  defp stdlib_ext_module?(name), do: name in @stdlib_ext_modules

  defp stdlib_ext_path(module_name), do: Path.join(@stdlib_ext_dir, "#{module_name}.bl")

  defp load_ext_modules_for_imports(user_ast, stdlib_ast) do
    import_names =
      case user_ast do
        {:program, %{imports: imports}} ->
          Enum.map(imports, fn {:import, %{module: m}} -> m end)

        _ ->
          []
      end

    ext_needed = Enum.filter(import_names, &stdlib_ext_module?/1)

    Enum.reduce_while(ext_needed, {:ok, stdlib_ast}, fn mod_name, {:ok, acc} ->
      path = stdlib_ext_path(mod_name)

      case File.read(path) do
        {:ok, source} ->
          with {:ok, tokens} <- Lexer.tokenize(source, path),
               {:ok, ast} <- Parser.parse(tokens) do
            {:cont, {:ok, merge_programs_simple(acc, ast)}}
          else
            {:error, %BeamLang.Error{} = err} -> {:halt, {:error, [err]}}
            {:error, errs} when is_list(errs) -> {:halt, {:error, errs}}
          end

        {:error, reason} ->
          span = BeamLang.Span.new(path, 0, 0)

          {:halt,
           {:error,
            [BeamLang.Error.new(:type, "Failed to read ext stdlib '#{mod_name}': #{inspect(reason)}", span)]}}
      end
    end)
  end

  defp empty_program() do
    {:program,
     %{
       module: nil,
       imports: [],
       types: [],
       enums: [],
       errors: [],
       functions: [],
       span: BeamLang.Span.new("<stdlib>", 0, 0)
     }}
  end

  defp merge_programs_simple(
         {:program,
          %{types: acc_types, enums: acc_enums, errors: acc_errors, functions: acc_funcs} = acc},
         {:program, %{types: types, enums: enums, errors: errors, functions: funcs}}
       ) do
    {:program,
     %{
       acc
       | types: acc_types ++ types,
         enums: acc_enums ++ enums,
         errors: acc_errors ++ errors,
         functions: acc_funcs ++ funcs
     }}
  end

  # Handle programs without errors/enums field for backwards compatibility
  defp merge_programs_simple(
         {:program, %{types: acc_types, functions: acc_funcs} = acc},
         {:program, %{types: types, functions: funcs} = prog2}
       )
       when not is_map_key(acc, :errors) or not is_map_key(prog2, :errors) do
    acc_errors = Map.get(acc, :errors, [])
    acc_enums = Map.get(acc, :enums, [])
    errors = Map.get(prog2, :errors, [])
    enums = Map.get(prog2, :enums, [])

    {:program,
     %{
       acc
       | types: acc_types ++ types,
         enums: acc_enums ++ enums,
         errors: acc_errors ++ errors,
         functions: acc_funcs ++ funcs
     }}
  end

  defp merge_programs(
         {:program, %{types: std_types, functions: std_funcs} = std_prog},
         {:program,
          %{module: module, imports: imports, types: types, functions: functions, span: span} =
            user_prog}
       ) do
    std_errors = Map.get(std_prog, :errors, [])
    std_enums = Map.get(std_prog, :enums, [])
    user_errors = Map.get(user_prog, :errors, [])
    user_enums = Map.get(user_prog, :enums, [])

    {:ok,
     {:program,
      %{
        module: module,
        imports: imports,
        types: std_types ++ types,
        enums: std_enums ++ user_enums,
        errors: std_errors ++ user_errors,
        functions: std_funcs ++ functions,
        span: span
      }}}
  end

  defp load_modules(path) do
    path = Path.expand(path)
    entry_module = module_name_from_path(path)

    case do_load_module(path, %{}, []) do
      {:ok, modules, errors} ->
        case errors do
          [] -> {:ok, modules, entry_module}
          _ -> {:error, errors}
        end

      {:error, errors} ->
        {:error, errors}
    end
  end

  defp do_load_module(path, modules, errors) do
    module_name = module_name_from_path(path)

    if Map.has_key?(modules, module_name) do
      {:ok, modules, errors}
    else
      case File.read(path) do
        {:ok, source} ->
          with {:ok, tokens} <- Lexer.tokenize(source, path),
               {:ok, ast} <- Parser.parse(tokens) do
            ast = set_program_module(ast, module_name)
            modules = Map.put(modules, module_name, %{ast: ast, path: path})

            dep_modules = program_imports(ast) ++ qualified_module_refs(ast)

            {modules, errors} =
              Enum.reduce(dep_modules, {modules, errors}, fn {dep_module, span},
                                                             {acc_mods, acc_errs} ->
                dep_path = Path.join(Path.dirname(path), "#{dep_module}.bl")

                resolved_path =
                  cond do
                    File.exists?(dep_path) -> dep_path
                    stdlib_ext_module?(dep_module) -> stdlib_ext_path(dep_module)
                    true -> nil
                  end

                if resolved_path do
                  case do_load_module(resolved_path, acc_mods, acc_errs) do
                    {:ok, mods2, errs2} -> {mods2, errs2}
                    {:error, errs2} -> {acc_mods, acc_errs ++ errs2}
                  end
                else
                  err = BeamLang.Error.new(:type, "Unknown module '#{dep_module}'.", span)
                  {acc_mods, [err | acc_errs]}
                end
              end)

            {:ok, modules, errors}
          else
            {:error, %BeamLang.Error{} = err} ->
              {:ok, modules, [err | errors]}

            {:error, errs} when is_list(errs) ->
              {:ok, modules, errs ++ errors}
          end

        {:error, reason} ->
          span = BeamLang.Span.new(path, 0, 0)
          err = BeamLang.Error.new(:type, "Failed to read file: #{inspect(reason)}", span)
          {:ok, modules, [err | errors]}
      end
    end
  end

  defp compile_modules(modules, stdlib_ast, entry_module) do
    exports = module_exports(modules)

    Enum.reduce_while(
      modules,
      {:ok, %{modules: [], entry: module_atom(entry_module)}},
      fn {module_name, %{ast: ast}}, {:ok, acc} ->
        case resolve_imports_and_qualify(ast, exports, module_name) do
          {:ok, resolved} ->
            with {:ok, full_stdlib} <- load_ext_modules_for_imports(resolved, stdlib_ast),
                 {:ok, merged} <- merge_programs(full_stdlib, resolved),
                 {:ok, checked} <-
                   Semantic.validate(merged, require_main: module_name == entry_module) do
              forms = Codegen.to_erlang_forms(checked)

              case Runtime.compile_forms(forms) do
                {:ok, mod, bin} ->
                  {:cont, {:ok, %{acc | modules: [{mod, bin} | acc.modules]}}}

                {:error, reason} ->
                  err =
                    BeamLang.Error.new(
                      :type,
                      "BEAM compile failed: #{inspect(reason)}",
                      BeamLang.Span.new("<source>", 0, 0)
                    )

                  {:halt, {:error, [err]}}
              end
            else
              {:error, errs} -> {:halt, {:error, errs}}
            end

          {:error, errs} ->
            {:halt, {:error, errs}}
        end
      end
    )
  end

  defp set_program_module(
         {:program, %{imports: imports, types: types, functions: functions, span: span} = prog},
         module
       ) do
    errors = Map.get(prog, :errors, [])

    {:program,
     %{
       module: module,
       imports: imports,
       types: types,
       errors: errors,
       functions: functions,
       span: span
     }}
  end

  defp program_imports({:program, %{imports: imports}}) do
    Enum.map(imports, fn {:import, %{module: module, span: span}} -> {module, span} end)
  end

  defp import_alias_map(imports) do
    imports
    |> Enum.reduce(%{}, fn {:import, %{module: module, alias: alias_name}}, acc ->
      case alias_name do
        nil -> acc
        _ -> Map.put(acc, alias_name, module)
      end
    end)
  end

  defp qualified_module_refs({:program, %{imports: imports}} = program) do
    alias_map = import_alias_map(imports)
    qualified = collect_qualified_refs(program)

    (qualified.types ++ qualified.functions)
    |> Enum.reduce(%{}, fn {name, span}, acc ->
      case split_qualified(name, alias_map) do
        {_, mod} -> Map.put_new(acc, mod, span)
        nil -> acc
      end
    end)
    |> Enum.map(fn {module, span} -> {module, span} end)
  end

  defp module_name_from_path(path) do
    path
    |> Path.basename()
    |> Path.rootname()
  end

  defp module_atom(name) when is_binary(name), do: String.to_atom(name)

  defp module_exports(modules) do
    Enum.reduce(modules, %{}, fn {module_name, %{ast: ast}}, acc ->
      {:program, %{types: types, functions: functions}} = ast

      type_defs =
        types
        |> Enum.filter(fn {:type_def, %{exported: exported}} -> exported end)
        |> Enum.map(fn {:type_def, %{name: name, params: params, fields: fields}} ->
          {export_name(name, module_name), %{params: params, fields: fields}}
        end)
        |> Map.new()

      func_defs =
        functions
        |> Enum.filter(fn {:function, %{exported: exported, body: body, internal: internal}} ->
          exported and body != nil and not internal
        end)
        |> Enum.map(fn {:function, %{name: name, params: params, return_type: return_type}} ->
          param_types = Enum.map(params, & &1.type)
          {export_name(name, module_name), {param_types, return_type}}
        end)
        |> Map.new()

      type_exports = Map.keys(type_defs) |> MapSet.new()
      func_exports = Map.keys(func_defs) |> MapSet.new()

      Map.put(acc, module_name, %{
        types: type_exports,
        functions: func_exports,
        type_defs: type_defs,
        func_defs: func_defs
      })
    end)
  end

  defp export_name(name, module_name) when is_binary(name) do
    normalize_export_name(name, module_name)
  end

  defp resolve_imports_and_qualify(
         {:program,
          %{module: module, imports: imports, types: types, functions: functions, span: span} =
            prog},
         exports,
         module_name
       ) do
    errors_list = Map.get(prog, :errors, [])
    local_types = MapSet.new(Enum.map(types, fn {:type_def, %{name: name}} -> name end))
    local_funcs = MapSet.new(Enum.map(functions, fn {:function, %{name: name}} -> name end))
    alias_map = import_alias_map(imports)

    {import_type_map, import_func_map, import_errors} =
      resolve_imports(imports, exports, local_types, local_funcs)

    {qualified_imports, qualified_errors} =
      qualified_imports_from_program(
        {:program, %{types: types, functions: functions}},
        exports,
        module_name,
        alias_map
      )

    {import_type_map, type_conflicts} =
      merge_import_maps(import_type_map, qualified_imports.types, :type)

    {import_func_map, func_conflicts} =
      merge_import_maps(import_func_map, qualified_imports.functions, :function)

    fallback_maps =
      qualified_maps_from_program({:program, %{types: types, functions: functions}}, alias_map)

    {import_type_map, fb_type_conflicts} =
      merge_import_maps(import_type_map, fallback_maps.types, :type)

    {import_func_map, fb_func_conflicts} =
      merge_import_maps(import_func_map, fallback_maps.functions, :function)

    {import_type_map, import_func_map, module_conflicts} =
      add_qualified_module_exports(
        {:program, %{types: types, functions: functions}},
        exports,
        local_types,
        local_funcs,
        import_type_map,
        import_func_map,
        alias_map
      )

    errors =
      import_errors ++
        qualified_errors ++
        type_conflicts ++
        func_conflicts ++ fb_type_conflicts ++ fb_func_conflicts ++ module_conflicts

    if errors != [] do
      {:error, errors}
    else
      {type_stubs, func_stubs} = import_stubs(import_type_map, import_func_map, exports)
      types = type_stubs ++ types
      functions = func_stubs ++ functions

      types = Enum.map(types, &qualify_type_def(&1, import_type_map, local_types, alias_map))

      functions =
        Enum.map(
          functions,
          &qualify_function(
            &1,
            import_func_map,
            import_type_map,
            local_types,
            local_funcs,
            alias_map
          )
        )

      {:ok,
       {:program,
        %{
          module: module,
          imports: imports,
          types: types,
          errors: errors_list,
          functions: functions,
          span: span
        }}}
    end
  end

  defp qualified_imports_from_program(program, exports, module_name, alias_map) do
    qualified = collect_qualified_refs(program)

    {types, type_errors} =
      resolve_qualified_items(qualified.types, exports, module_name, alias_map)

    {functions, func_errors} =
      resolve_qualified_items(qualified.functions, exports, module_name, alias_map)

    {%{types: types, functions: functions}, type_errors ++ func_errors}
  end

  defp qualified_maps_from_program({:program, %{types: types, functions: functions}}, alias_map) do
    type_names =
      Enum.flat_map(types, fn {:type_def, %{fields: fields}} ->
        Enum.flat_map(fields, fn %{type: type} -> qualified_in_type(type) end)
      end)

    func_names =
      Enum.flat_map(functions, fn {:function,
                                   %{params: params, return_type: return_type, body: body}} ->
        type_refs =
          Enum.flat_map(params, fn %{type: type} -> qualified_in_type(type) end) ++
            qualified_in_type(return_type)

        expr_refs =
          case body do
            nil -> []
            _ -> qualified_in_expr(body)
          end

        type_refs ++ expr_refs
      end)

    type_map =
      type_names
      |> Enum.map(&split_qualified(&1, alias_map))
      |> Enum.reject(&is_nil/1)
      |> Map.new()

    func_map =
      func_names
      |> Enum.map(&split_qualified(&1, alias_map))
      |> Enum.reject(&is_nil/1)
      |> Map.new()

    %{types: type_map, functions: func_map}
  end

  defp add_qualified_module_exports(
         program,
         exports,
         local_types,
         local_funcs,
         type_map,
         func_map,
         alias_map
       ) do
    modules = qualified_module_names(program, alias_map)

    Enum.reduce(modules, {type_map, func_map, []}, fn module, {t_map, f_map, errors} ->
      case Map.fetch(exports, module) do
        :error ->
          {t_map, f_map, errors}

        {:ok, %{type_defs: type_defs, func_defs: func_defs}} ->
          {t_map, errors} =
            Enum.reduce(export_keys(type_defs, module), {t_map, errors}, fn name, {acc, errs} ->
              cond do
                MapSet.member?(local_types, name) -> {acc, errs}
                Map.has_key?(acc, name) -> {acc, errs}
                true -> {Map.put(acc, name, module), errs}
              end
            end)

          {f_map, errors} =
            Enum.reduce(export_keys(func_defs, module), {f_map, errors}, fn name, {acc, errs} ->
              cond do
                MapSet.member?(local_funcs, name) -> {acc, errs}
                Map.has_key?(acc, name) -> {acc, errs}
                true -> {Map.put(acc, name, module), errs}
              end
            end)

          {t_map, f_map, errors}
      end
    end)
  end

  defp qualified_module_names(program, alias_map) do
    qualified = collect_qualified_refs(program)

    (qualified.types ++ qualified.functions)
    |> Enum.map(fn {name, _span} -> split_qualified(name, alias_map) end)
    |> Enum.reject(&is_nil/1)
    |> Enum.map(fn {_item, mod} -> mod end)
    |> Enum.uniq()
  end

  defp qualified_in_type({:named, name}) when is_binary(name) do
    if String.contains?(name, "::"), do: [name], else: []
  end

  defp qualified_in_type({:generic, base, args}),
    do: qualified_in_type(base) ++ Enum.flat_map(args, &qualified_in_type/1)

  defp qualified_in_type({:optional, inner}), do: qualified_in_type(inner)

  defp qualified_in_type({:result, ok_t, err_t}),
    do: qualified_in_type(ok_t) ++ qualified_in_type(err_t)

  defp qualified_in_type({:fn, params, return_type}),
    do: Enum.flat_map(params, &qualified_in_type/1) ++ qualified_in_type(return_type)

  defp qualified_in_type(_), do: []

  defp qualified_in_expr({:block, %{stmts: stmts}}),
    do: Enum.flat_map(stmts, &qualified_in_stmt/1)

  defp qualified_in_expr({:block_expr, %{block: block}}), do: qualified_in_expr(block)

  defp qualified_in_expr({:call, %{name: name, args: args}}) do
    refs = if String.contains?(name, "::"), do: [name], else: []
    refs ++ Enum.flat_map(args, &qualified_in_expr/1)
  end

  defp qualified_in_expr({:match, %{expr: expr, cases: cases}}) do
    qualified_in_expr(expr) ++ Enum.flat_map(cases, &qualified_in_case/1)
  end

  defp qualified_in_expr(
         {:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch}}
       ) do
    qualified_in_expr(cond) ++ qualified_in_expr(then_block) ++ qualified_in_else(else_branch)
  end

  defp qualified_in_expr({:binary, %{left: left, right: right}}),
    do: qualified_in_expr(left) ++ qualified_in_expr(right)

  defp qualified_in_expr({:field, %{target: target}}), do: qualified_in_expr(target)
  defp qualified_in_expr({:opt_some, %{expr: expr}}), do: qualified_in_expr(expr)
  defp qualified_in_expr({:res_ok, %{expr: expr}}), do: qualified_in_expr(expr)
  defp qualified_in_expr({:res_err, %{expr: expr}}), do: qualified_in_expr(expr)

  defp qualified_in_expr({:lambda, %{params: params, return_type: return_type, body: body}}) do
    param_types = Enum.flat_map(params, fn %{type: type} -> qualified_in_type(type) end)
    param_types ++ qualified_in_type(return_type) ++ qualified_in_expr(body)
  end

  defp qualified_in_expr({:method_call, %{target: target, args: args}}) do
    qualified_in_expr(target) ++ Enum.flat_map(args, &qualified_in_expr/1)
  end

  defp qualified_in_expr(_), do: []

  defp qualified_in_stmt({:let, %{expr: expr, type: type}}) do
    qualified_in_type(type) ++ qualified_in_expr(expr)
  end

  defp qualified_in_stmt({:assign, %{target: target, expr: expr}}),
    do: qualified_in_expr(target) ++ qualified_in_expr(expr)

  defp qualified_in_stmt({:expr, %{expr: expr}}), do: qualified_in_expr(expr)
  defp qualified_in_stmt({:return, %{expr: nil}}), do: []
  defp qualified_in_stmt({:return, %{expr: expr}}), do: qualified_in_expr(expr)

  defp qualified_in_stmt({:guard, %{cond: cond, else_block: else_block}}),
    do: qualified_in_expr(cond) ++ qualified_in_expr(else_block)

  defp qualified_in_stmt(
         {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}}
       ),
       do:
         qualified_in_expr(cond) ++
           qualified_in_expr(then_block) ++ qualified_in_else(else_branch)

  defp qualified_in_stmt({:while, %{cond: cond, body: body}}),
    do: qualified_in_expr(cond) ++ qualified_in_expr(body)

  defp qualified_in_stmt({:loop, %{body: body}}), do: qualified_in_expr(body)

  defp qualified_in_stmt({:for, %{collection: collection, body: body}}),
    do: qualified_in_expr(collection) ++ qualified_in_expr(body)

  defp qualified_in_stmt(_), do: []

  defp qualified_in_case(%{pattern: pattern, guard: guard, body: body}) do
    qualified_in_pattern(pattern) ++
      if(guard == nil, do: [], else: qualified_in_expr(guard)) ++
      qualified_in_expr(body)
  end

  defp qualified_in_pattern({:struct_pattern, %{name: name, fields: fields}}) do
    refs = if String.contains?(name, "::"), do: [name], else: []
    refs ++ Enum.flat_map(fields, fn %{pattern: pat} -> qualified_in_pattern(pat) end)
  end

  defp qualified_in_pattern(_), do: []

  defp qualified_in_else(nil), do: []
  defp qualified_in_else({:else_block, %{block: block}}), do: qualified_in_expr(block)
  defp qualified_in_else({:else_if, %{if: if_stmt}}), do: qualified_in_stmt(if_stmt)

  defp split_qualified(name, alias_map) when is_binary(name) do
    case String.split(name, "::", parts: 2) do
      [mod, item] -> {item, Map.get(alias_map, mod, mod)}
      _ -> nil
    end
  end

  defp merge_import_maps(primary, secondary, kind) do
    {merged, conflicts} =
      Enum.reduce(secondary, {primary, []}, fn {name, module}, {acc, errs} ->
        case Map.fetch(acc, name) do
          {:ok, other} when other != module ->
            err =
              BeamLang.Error.new(
                :type,
                "Imported #{kind} '#{name}' conflicts with import from #{other}.",
                BeamLang.Span.new("<import>", 0, 0)
              )

            {acc, [err | errs]}

          _ ->
            {Map.put(acc, name, module), errs}
        end
      end)

    {merged, conflicts}
  end

  defp resolve_imports(imports, exports, local_types, local_funcs) do
    Enum.reduce(imports, {%{}, %{}, []}, fn {:import, %{module: module, items: items, span: span}},
                                            {type_map, func_map, errors} ->
      case Map.fetch(exports, module) do
        :error ->
          err = BeamLang.Error.new(:type, "Unknown module '#{module}'.", span)
          {type_map, func_map, [err | errors]}

        {:ok,
         %{
           type_defs: type_defs,
           func_defs: func_defs,
           types: _type_exports,
           functions: _func_exports
         }} ->
          {items, errors} =
            case items do
              :all ->
                all_items =
                  export_keys(type_defs, module)
                  |> Enum.map(fn name -> %{name: name, span: span} end)
                  |> Kernel.++(
                    export_keys(func_defs, module)
                    |> Enum.map(fn name -> %{name: name, span: span} end)
                  )

                {all_items, errors}

              :none ->
                {[], errors}

              list ->
                {list, errors}
            end

          Enum.reduce(items, {type_map, func_map, errors}, fn %{name: name, span: item_span},
                                                              {t_map, f_map, errs} ->
            cond do
              has_export_key?(type_defs, module, name) and
                  has_export_key?(func_defs, module, name) ->
                err =
                  BeamLang.Error.new(
                    :type,
                    "Import '#{name}' is both a type and function in #{module}.",
                    item_span
                  )

                {t_map, f_map, [err | errs]}

              has_export_key?(type_defs, module, name) ->
                if MapSet.member?(local_types, name) do
                  err =
                    BeamLang.Error.new(
                      :type,
                      "Imported type '#{name}' conflicts with local type.",
                      item_span
                    )

                  {t_map, f_map, [err | errs]}
                else
                  case Map.fetch(t_map, name) do
                    {:ok, other_mod} when other_mod != module ->
                      err =
                        BeamLang.Error.new(
                          :type,
                          "Imported type '#{name}' conflicts with import from #{other_mod}.",
                          item_span
                        )

                      {t_map, f_map, [err | errs]}

                    _ ->
                      {Map.put(t_map, name, module), f_map, errs}
                  end
                end

              has_export_key?(func_defs, module, name) ->
                if MapSet.member?(local_funcs, name) do
                  err =
                    BeamLang.Error.new(
                      :type,
                      "Imported function '#{name}' conflicts with local function.",
                      item_span
                    )

                  {t_map, f_map, [err | errs]}
                else
                  case Map.fetch(f_map, name) do
                    {:ok, other_mod} when other_mod != module ->
                      err =
                        BeamLang.Error.new(
                          :type,
                          "Imported function '#{name}' conflicts with import from #{other_mod}.",
                          item_span
                        )

                      {t_map, f_map, [err | errs]}

                    _ ->
                      {t_map, Map.put(f_map, name, module), errs}
                  end
                end

              true ->
                err =
                  BeamLang.Error.new(:type, "Unknown import '#{name}' from #{module}.", item_span)

                {t_map, f_map, [err | errs]}
            end
          end)
      end
    end)
  end

  defp resolve_qualified_items(items, exports, module_name, alias_map) do
    Enum.reduce(items, {%{}, []}, fn {name, span}, {acc, errors} ->
      case split_qualified(name, alias_map) do
        {item, mod} ->
          if mod == module_name do
            {acc, errors}
          else
            case Map.fetch(exports, mod) do
              :error ->
                err = BeamLang.Error.new(:type, "Unknown module '#{mod}'.", span)
                {acc, [err | errors]}

              {:ok,
               %{
                 type_defs: _type_defs,
                 func_defs: _func_defs,
                 types: _type_exports,
                 functions: _func_exports
               }} ->
                case Map.fetch(acc, item) do
                  {:ok, other_mod} when other_mod != mod ->
                    err =
                      BeamLang.Error.new(
                        :type,
                        "Qualified reference '#{item}' conflicts with import from #{other_mod}.",
                        span
                      )

                    {acc, [err | errors]}

                  _ ->
                    {Map.put(acc, item, mod), errors}
                end
            end
          end

        nil ->
          {acc, errors}
      end
    end)
  end

  defp import_stubs(type_map, func_map, exports) do
    type_stubs =
      Enum.map(type_map, fn {name, module} ->
        {:type_def,
         %{
           name: qualified_name(module, name),
           params: [],
           fields: [],
           exported: false,
           span: BeamLang.Span.new("<import>", 0, 0)
         }}
        |> attach_type_fields(exports, module, name)
      end)

    func_stubs =
      Enum.map(func_map, fn {name, module} ->
        {_param_types, _ret_type} = export_signature(exports, module, name)

        {:function,
         %{
           name: qualified_name(module, name),
           params: export_params(exports, module, name),
           return_type: export_return(exports, module, name),
           body: nil,
           external: nil,
           exported: false,
           internal: false,
           span: BeamLang.Span.new("<import>", 0, 0)
         }}
      end)

    {type_stubs, func_stubs}
  end

  defp attach_type_fields(
         {:type_def, %{name: qualified, params: _params} = info} = defn,
         exports,
         module,
         name
       ) do
    case Map.fetch(exports, module) do
      {:ok, %{type_defs: type_defs}} ->
        case fetch_export(type_defs, module, name) do
          {:ok, %{params: params, fields: fields}} ->
            fields =
              Enum.map(fields, fn %{name: field_name, type: type, span: span} = field ->
                %{
                  name: field_name,
                  type: qualify_type_in_module(type, module, exports),
                  span: span,
                  internal: Map.get(field, :internal, false)
                }
              end)

            {:type_def, %{info | name: qualified, params: params, fields: fields}}

          :error ->
            defn
        end

      _ ->
        defn
    end
  end

  defp export_signature(exports, module, name) do
    case Map.fetch(exports, module) do
      {:ok, %{func_defs: func_defs}} ->
        case fetch_export(func_defs, module, name) do
          {:ok, {param_types, return_type}} ->
            param_types = Enum.map(param_types, &qualify_type_in_module(&1, module, exports))
            return_type = qualify_type_in_module(return_type, module, exports)
            {param_types, return_type}

          :error ->
            {[], :void}
        end

      _ ->
        {[], :void}
    end
  end

  defp qualify_type_in_module(type, module, exports) do
    case type do
      {:named, name} ->
        if String.contains?(name, "::") do
          {:named, name}
        else
          case Map.fetch(exports, module) do
            {:ok, %{type_defs: type_defs}} ->
              matches =
                type_defs
                |> Map.keys()
                |> Enum.filter(fn key -> normalize_export_name(key, module) == name end)

              if Map.has_key?(type_defs, name) or
                   Map.has_key?(type_defs, qualified_name(module, name)) or
                   matches != [] do
                {:named, qualified_name(module, name)}
              else
                {:named, name}
              end

            _ ->
              {:named, name}
          end
        end

      {:generic, base, args} ->
        base = qualify_type_in_module(base, module, exports)
        args = Enum.map(args, &qualify_type_in_module(&1, module, exports))
        {:generic, base, args}

      {:optional, inner} ->
        {:optional, qualify_type_in_module(inner, module, exports)}

      {:result, ok_t, err_t} ->
        {:result, qualify_type_in_module(ok_t, module, exports),
         qualify_type_in_module(err_t, module, exports)}

      _ ->
        type
    end
  end

  defp export_params(exports, module, name) do
    {param_types, _} = export_signature(exports, module, name)

    Enum.with_index(param_types)
    |> Enum.map(fn {type, idx} ->
      %{
        name: "arg#{idx + 1}",
        type: type,
        mutable: false,
        span: BeamLang.Span.new("<import>", 0, 0)
      }
    end)
  end

  defp export_return(exports, module, name) do
    {_param_types, return_type} = export_signature(exports, module, name)
    return_type
  end

  defp export_keys(defs, module) do
    defs
    |> Map.keys()
    |> Enum.map(&normalize_export_name(&1, module))
    |> Enum.uniq()
  end

  defp has_export_key?(defs, module, name) do
    Map.has_key?(defs, name) or
      Map.has_key?(defs, qualified_name(module, name)) or
      Enum.any?(Map.keys(defs), fn key -> normalize_export_name(key, module) == name end)
  end

  defp fetch_export(defs, module, name) do
    case Map.fetch(defs, name) do
      {:ok, value} ->
        {:ok, value}

      :error ->
        case Map.fetch(defs, qualified_name(module, name)) do
          {:ok, value} ->
            {:ok, value}

          :error ->
            case Enum.find(Map.keys(defs), fn key ->
                   normalize_export_name(key, module) == name
                 end) do
              nil -> :error
              key -> Map.fetch(defs, key)
            end
        end
    end
  end

  defp normalize_export_name(name, module) when is_binary(name) do
    _ = module

    case String.split(name, "::") do
      [] -> name
      parts -> List.last(parts)
    end
  end

  defp qualified_name(module, name), do: "#{module}::#{name}"

  defp resolve_alias_in_qualified(name, alias_map) when is_binary(name) do
    case String.split(name, "::", parts: 2) do
      [mod, item] -> "#{Map.get(alias_map, mod, mod)}::#{item}"
      _ -> name
    end
  end

  defp qualify_type_def({:type_def, %{fields: fields} = info}, type_map, local_types, alias_map) do
    fields =
      Enum.map(fields, fn %{name: field_name, type: field_type, span: span} = field ->
        %{
          name: field_name,
          type: qualify_type(field_type, type_map, local_types, alias_map),
          span: span,
          internal: Map.get(field, :internal, false),
          annotations: Map.get(field, :annotations, [])
        }
      end)

    {:type_def, %{info | fields: fields}}
  end

  defp qualify_function(
         {:function, %{params: params, return_type: return_type, body: body} = info},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    params =
      Enum.map(params, fn param ->
        case param do
          %{name: name, type: type} ->
            %{
              name: name,
              type: qualify_type(type, type_map, local_types, alias_map),
              mutable: Map.get(param, :mutable, false),
              span: param.span
            }

          %{pattern: pattern, type: type} ->
            %{
              pattern: pattern,
              type: qualify_type(type, type_map, local_types, alias_map),
              span: param.span
            }
        end
      end)

    return_type = qualify_type(return_type, type_map, local_types, alias_map)

    body =
      case body do
        nil -> nil
        _ -> qualify_block(body, func_map, type_map, local_types, local_funcs, alias_map)
      end

    {:function, %{info | params: params, return_type: return_type, body: body}}
  end

  defp qualify_block(
         {:block, %{stmts: stmts, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    stmts =
      Enum.map(stmts, &qualify_stmt(&1, func_map, type_map, local_types, local_funcs, alias_map))

    {:block, %{stmts: stmts, span: span}}
  end

  defp qualify_stmt(
         {:let, %{name: name, mutable: mutable, type: type, expr: expr, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    type = if type == nil, do: nil, else: qualify_type(type, type_map, local_types, alias_map)
    expr = qualify_expr(expr, func_map, type_map, local_types, local_funcs, alias_map)
    {:let, %{name: name, mutable: mutable, type: type, expr: expr, span: span}}
  end

  defp qualify_stmt(
         {:assign, %{target: target, expr: expr, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    target = qualify_expr(target, func_map, type_map, local_types, local_funcs, alias_map)
    expr = qualify_expr(expr, func_map, type_map, local_types, local_funcs, alias_map)
    {:assign, %{target: target, expr: expr, span: span}}
  end

  defp qualify_stmt(
         {:expr, %{expr: expr, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    {:expr,
     %{
       expr: qualify_expr(expr, func_map, type_map, local_types, local_funcs, alias_map),
       span: span
     }}
  end

  defp qualify_stmt(
         {:return, %{expr: expr, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    expr =
      if expr == nil,
        do: nil,
        else: qualify_expr(expr, func_map, type_map, local_types, local_funcs, alias_map)

    {:return, %{expr: expr, span: span}}
  end

  defp qualify_stmt(
         {:guard, %{cond: cond, else_block: else_block, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    cond = qualify_expr(cond, func_map, type_map, local_types, local_funcs, alias_map)

    else_block =
      qualify_block(else_block, func_map, type_map, local_types, local_funcs, alias_map)

    {:guard, %{cond: cond, else_block: else_block, span: span}}
  end

  defp qualify_stmt(
         {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    cond = qualify_expr(cond, func_map, type_map, local_types, local_funcs, alias_map)

    then_block =
      qualify_block(then_block, func_map, type_map, local_types, local_funcs, alias_map)

    else_branch =
      qualify_else_branch(else_branch, func_map, type_map, local_types, local_funcs, alias_map)

    {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch, span: span}}
  end

  defp qualify_stmt(
         {:while, %{cond: cond, body: body, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    cond = qualify_expr(cond, func_map, type_map, local_types, local_funcs, alias_map)
    body = qualify_block(body, func_map, type_map, local_types, local_funcs, alias_map)
    {:while, %{cond: cond, body: body, span: span}}
  end

  defp qualify_stmt(
         {:loop, %{body: body, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    {:loop,
     %{
       body: qualify_block(body, func_map, type_map, local_types, local_funcs, alias_map),
       span: span
     }}
  end

  defp qualify_stmt(
         {:for, %{collection: collection, body: body, span: span} = info},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    collection = qualify_expr(collection, func_map, type_map, local_types, local_funcs, alias_map)
    body = qualify_block(body, func_map, type_map, local_types, local_funcs, alias_map)
    {:for, %{info | collection: collection, body: body, span: span}}
  end

  defp qualify_stmt(stmt, _func_map, _type_map, _local_types, _local_funcs, _alias_map), do: stmt

  defp qualify_else_branch(nil, _func_map, _type_map, _local_types, _local_funcs, _alias_map),
    do: nil

  defp qualify_else_branch(
         {:else_block, %{block: block, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    {:else_block,
     %{
       block: qualify_block(block, func_map, type_map, local_types, local_funcs, alias_map),
       span: span
     }}
  end

  defp qualify_else_branch(
         {:else_if, %{if: if_stmt, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    {:else_if,
     %{
       if: qualify_stmt(if_stmt, func_map, type_map, local_types, local_funcs, alias_map),
       span: span
     }}
  end

  defp qualify_expr(
         {:call, %{name: name, args: args, span: span} = info},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    args =
      Enum.map(args, &qualify_expr(&1, func_map, type_map, local_types, local_funcs, alias_map))

    type_args =
      info
      |> Map.get(:type_args, [])
      |> Enum.map(&qualify_type(&1, type_map, local_types, alias_map))

    name =
      if String.contains?(name, "::") do
        resolve_alias_in_qualified(name, alias_map)
      else
        cond do
          MapSet.member?(local_funcs, name) ->
            name

          true ->
            case Map.fetch(func_map, name) do
              {:ok, module} -> qualified_name(module, name)
              :error -> name
            end
        end
      end

    {:call, %{name: name, args: args, span: span, type_args: type_args, type_info: nil}}
  end

  defp qualify_expr(
         {:identifier, _} = expr,
         _func_map,
         _type_map,
         _local_types,
         _local_funcs,
         _alias_map
       ),
       do: expr

  defp qualify_expr(
         {:struct, %{fields: fields, type: type, span: span} = info},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    fields =
      Enum.map(fields, fn %{name: field_name, expr: expr, span: field_span} ->
        %{
          name: field_name,
          expr: qualify_expr(expr, func_map, type_map, local_types, local_funcs, alias_map),
          span: field_span
        }
      end)

    type = if type == nil, do: nil, else: qualify_type(type, type_map, local_types, alias_map)
    # Preserve operators from original info
    operators = Map.get(info, :operators, [])
    {:struct, %{fields: fields, type: type, span: span, operators: operators}}
  end

  defp qualify_expr(
         {:field, %{target: target, name: name, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    {:field,
     %{
       target: qualify_expr(target, func_map, type_map, local_types, local_funcs, alias_map),
       name: name,
       span: span
     }}
  end

  defp qualify_expr(
         {:block_expr, %{block: block, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    {:block_expr,
     %{
       block: qualify_block(block, func_map, type_map, local_types, local_funcs, alias_map),
       span: span
     }}
  end

  defp qualify_expr(
         {:match, %{expr: expr, cases: cases, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    expr = qualify_expr(expr, func_map, type_map, local_types, local_funcs, alias_map)

    cases =
      Enum.map(
        cases,
        &qualify_match_case(&1, func_map, type_map, local_types, local_funcs, alias_map)
      )

    {:match, %{expr: expr, cases: cases, span: span}}
  end

  defp qualify_expr(
         {:binary, %{op: op, left: left, right: right, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    {:binary,
     %{
       op: op,
       left: qualify_expr(left, func_map, type_map, local_types, local_funcs, alias_map),
       right: qualify_expr(right, func_map, type_map, local_types, local_funcs, alias_map),
       span: span
     }}
  end

  defp qualify_expr(
         {:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    cond = qualify_expr(cond, func_map, type_map, local_types, local_funcs, alias_map)

    then_block =
      qualify_block(then_block, func_map, type_map, local_types, local_funcs, alias_map)

    else_branch =
      qualify_else_branch(else_branch, func_map, type_map, local_types, local_funcs, alias_map)

    {:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch, span: span}}
  end

  defp qualify_expr(
         {:opt_some, %{expr: expr, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    {:opt_some,
     %{
       expr: qualify_expr(expr, func_map, type_map, local_types, local_funcs, alias_map),
       span: span
     }}
  end

  defp qualify_expr(
         {:res_ok, %{expr: expr, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    {:res_ok,
     %{
       expr: qualify_expr(expr, func_map, type_map, local_types, local_funcs, alias_map),
       span: span
     }}
  end

  defp qualify_expr(
         {:res_err, %{expr: expr, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    {:res_err,
     %{
       expr: qualify_expr(expr, func_map, type_map, local_types, local_funcs, alias_map),
       span: span
     }}
  end

  defp qualify_expr(
         {:lambda, %{params: params, return_type: return_type, body: body, span: span}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    params =
      Enum.map(params, fn param ->
        %{
          name: param.name,
          type: qualify_type(param.type, type_map, local_types, alias_map),
          mutable: Map.get(param, :mutable, false),
          span: param.span
        }
      end)

    return_type = qualify_type(return_type, type_map, local_types, alias_map)
    body = qualify_expr(body, func_map, type_map, local_types, local_funcs, alias_map)
    {:lambda, %{params: params, return_type: return_type, body: body, span: span}}
  end

  defp qualify_expr(
         {:method_call,
          %{target: target, name: name, args: args, span: span, target_type: target_type}},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    # add debug info
    target = qualify_expr(target, func_map, type_map, local_types, local_funcs, alias_map)

    args =
      Enum.map(args, &qualify_expr(&1, func_map, type_map, local_types, local_funcs, alias_map))

    {:method_call,
     %{target: target, name: name, args: args, span: span, target_type: target_type}}
  end

  defp qualify_expr(expr, _func_map, _type_map, _local_types, _local_funcs, _alias_map), do: expr

  defp qualify_match_case(
         %{pattern: pattern, guard: guard, body: body, span: span},
         func_map,
         type_map,
         local_types,
         local_funcs,
         alias_map
       ) do
    pattern = qualify_pattern(pattern, type_map, local_types, alias_map)

    guard =
      if guard == nil,
        do: nil,
        else: qualify_expr(guard, func_map, type_map, local_types, local_funcs, alias_map)

    body = qualify_expr(body, func_map, type_map, local_types, local_funcs, alias_map)
    %{pattern: pattern, guard: guard, body: body, span: span}
  end

  defp qualify_pattern(
         {:struct_pattern, %{name: name, fields: fields, span: span}},
         type_map,
         local_types,
         alias_map
       ) do
    name =
      if String.contains?(name, "::") do
        resolve_alias_in_qualified(name, alias_map)
      else
        cond do
          MapSet.member?(local_types, name) ->
            name

          true ->
            case Map.fetch(type_map, name) do
              {:ok, module} -> qualified_name(module, name)
              :error -> name
            end
        end
      end

    fields =
      Enum.map(fields, fn %{name: field_name, pattern: pattern, span: field_span} ->
        %{
          name: field_name,
          pattern: qualify_pattern(pattern, type_map, local_types, alias_map),
          span: field_span
        }
      end)

    {:struct_pattern, %{name: name, fields: fields, span: span}}
  end

  defp qualify_pattern(pattern, _type_map, _local_types, _alias_map) do
    case pattern do
      {:opt_some_pat, _} -> pattern
      {:opt_none_pat, _} -> pattern
      {:res_ok_pat, _} -> pattern
      {:res_err_pat, _} -> pattern
      {:pat_identifier, _} -> pattern
      {:wildcard, _} -> pattern
      {:integer, _} -> pattern
      {:float, _} -> pattern
      {:string, _} -> pattern
      {:char, _} -> pattern
      {:bool, _} -> pattern
      _ -> pattern
    end
  end

  defp qualify_type(type, type_map, local_types, alias_map) do
    case type do
      {:named, name} ->
        if String.contains?(name, "::") do
          {:named, resolve_alias_in_qualified(name, alias_map)}
        else
          cond do
            MapSet.member?(local_types, name) ->
              {:named, name}

            true ->
              case Map.fetch(type_map, name) do
                {:ok, module} -> {:named, qualified_name(module, name)}
                :error -> {:named, name}
              end
          end
        end

      {:generic, base, args} ->
        base = qualify_type(base, type_map, local_types, alias_map)
        args = Enum.map(args, &qualify_type(&1, type_map, local_types, alias_map))
        {:generic, base, args}

      {:optional, inner} ->
        {:optional, qualify_type(inner, type_map, local_types, alias_map)}

      {:result, ok_t, err_t} ->
        {:result, qualify_type(ok_t, type_map, local_types, alias_map),
         qualify_type(err_t, type_map, local_types, alias_map)}

      {:fn, params, return_type} ->
        params = Enum.map(params, &qualify_type(&1, type_map, local_types, alias_map))
        return_type = qualify_type(return_type, type_map, local_types, alias_map)
        {:fn, params, return_type}

      _ ->
        type
    end
  end

  defp collect_qualified_refs({:program, %{types: types, functions: functions}}) do
    types_refs =
      types
      |> Enum.flat_map(&collect_type_refs(&1))
      |> Enum.filter(&String.contains?(elem(&1, 0), "::"))

    func_refs =
      functions
      |> Enum.flat_map(&collect_func_refs(&1))
      |> Enum.filter(&String.contains?(elem(&1, 0), "::"))

    %{types: types_refs, functions: func_refs}
  end

  defp collect_type_refs({:type_def, %{fields: fields}}) do
    Enum.flat_map(fields, fn %{type: type, span: span} -> collect_type_name_refs(type, span) end)
  end

  defp collect_func_refs(
         {:function, %{params: params, return_type: return_type, body: body, span: span}}
       ) do
    type_refs =
      Enum.flat_map(params, fn %{type: type, span: param_span} ->
        collect_type_name_refs(type, param_span)
      end) ++
        collect_type_name_refs(return_type, span)

    expr_refs =
      case body do
        nil -> []
        _ -> collect_expr_refs(body)
      end

    type_refs ++ expr_refs
  end

  defp collect_type_name_refs({:named, name}, span) when is_binary(name), do: [{name, span}]

  defp collect_type_name_refs({:generic, base, args}, span) do
    collect_type_name_refs(base, span) ++ Enum.flat_map(args, &collect_type_name_refs(&1, span))
  end

  defp collect_type_name_refs({:optional, inner}, span), do: collect_type_name_refs(inner, span)

  defp collect_type_name_refs({:result, ok_t, err_t}, span),
    do: collect_type_name_refs(ok_t, span) ++ collect_type_name_refs(err_t, span)

  defp collect_type_name_refs({:fn, params, return_type}, span),
    do:
      Enum.flat_map(params, &collect_type_name_refs(&1, span)) ++
        collect_type_name_refs(return_type, span)

  defp collect_type_name_refs(_type, _span), do: []

  defp collect_expr_refs({:block, %{stmts: stmts}}),
    do: Enum.flat_map(stmts, &collect_stmt_refs/1)

  defp collect_expr_refs({:call, %{name: name, span: span, args: args}}) do
    refs = if String.contains?(name, "::"), do: [{name, span}], else: []
    refs ++ Enum.flat_map(args, &collect_expr_refs/1)
  end

  defp collect_expr_refs({:match, %{expr: expr, cases: cases}}) do
    collect_expr_refs(expr) ++ Enum.flat_map(cases, &collect_case_refs/1)
  end

  defp collect_expr_refs({:block_expr, %{block: block}}), do: collect_expr_refs(block)

  defp collect_expr_refs(
         {:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch}}
       ) do
    collect_expr_refs(cond) ++ collect_expr_refs(then_block) ++ collect_else_refs(else_branch)
  end

  defp collect_expr_refs({:binary, %{left: left, right: right}}),
    do: collect_expr_refs(left) ++ collect_expr_refs(right)

  defp collect_expr_refs({:field, %{target: target}}), do: collect_expr_refs(target)
  defp collect_expr_refs({:opt_some, %{expr: expr}}), do: collect_expr_refs(expr)
  defp collect_expr_refs({:res_ok, %{expr: expr}}), do: collect_expr_refs(expr)
  defp collect_expr_refs({:res_err, %{expr: expr}}), do: collect_expr_refs(expr)

  defp collect_expr_refs(
         {:lambda, %{params: params, return_type: return_type, body: body, span: span}}
       ) do
    param_refs =
      Enum.flat_map(params, fn %{type: type} -> collect_type_name_refs(type, span) end)

    param_refs ++ collect_type_name_refs(return_type, span) ++ collect_expr_refs(body)
  end

  defp collect_expr_refs(_), do: []

  defp collect_stmt_refs({:let, %{expr: expr, type: type, span: span}}) do
    type_refs = if type == nil, do: [], else: collect_type_name_refs(type, span)
    type_refs ++ collect_expr_refs(expr)
  end

  defp collect_stmt_refs({:assign, %{target: target, expr: expr}}),
    do: collect_expr_refs(target) ++ collect_expr_refs(expr)

  defp collect_stmt_refs({:expr, %{expr: expr}}), do: collect_expr_refs(expr)
  defp collect_stmt_refs({:return, %{expr: nil}}), do: []
  defp collect_stmt_refs({:return, %{expr: expr}}), do: collect_expr_refs(expr)

  defp collect_stmt_refs({:guard, %{cond: cond, else_block: else_block}}),
    do: collect_expr_refs(cond) ++ collect_expr_refs(else_block)

  defp collect_stmt_refs(
         {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}}
       ),
       do:
         collect_expr_refs(cond) ++
           collect_expr_refs(then_block) ++ collect_else_refs(else_branch)

  defp collect_stmt_refs({:while, %{cond: cond, body: body}}),
    do: collect_expr_refs(cond) ++ collect_expr_refs(body)

  defp collect_stmt_refs({:loop, %{body: body}}), do: collect_expr_refs(body)

  defp collect_stmt_refs({:for, %{collection: collection, body: body}}),
    do: collect_expr_refs(collection) ++ collect_expr_refs(body)

  defp collect_stmt_refs(_), do: []

  defp collect_case_refs(%{pattern: pattern, guard: guard, body: body}) do
    collect_pattern_refs(pattern) ++
      if(guard == nil, do: [], else: collect_expr_refs(guard)) ++
      collect_expr_refs(body)
  end

  defp collect_pattern_refs({:struct_pattern, %{name: name, span: span, fields: fields}}) do
    refs = if String.contains?(name, "::"), do: [{name, span}], else: []
    refs ++ Enum.flat_map(fields, fn %{pattern: pat} -> collect_pattern_refs(pat) end)
  end

  defp collect_pattern_refs(_), do: []

  defp collect_else_refs(nil), do: []
  defp collect_else_refs({:else_block, %{block: block}}), do: collect_expr_refs(block)
  defp collect_else_refs({:else_if, %{if: if_stmt}}), do: collect_stmt_refs(if_stmt)

  @spec run_source(binary(), list()) :: {:ok, term()} | {:error, [BeamLang.Error.t()]}
  def run_source(source, args \\ []) when is_binary(source) do
    with {:ok, %{module: module, binary: binary}} <- compile_source(source),
         {:ok, value} <- Runtime.load_and_run(module, binary, args) do
      {:ok, value}
    end
  end

  @spec run_file(binary()) :: {:ok, term()} | {:error, [BeamLang.Error.t()]}
  def run_file(path) when is_binary(path) do
    case compile_file(path) do
      {:ok, %{entry: entry, modules: modules}} ->
        case Runtime.load_modules(modules) do
          :ok ->
            case List.keyfind(modules, entry, 0) do
              {^entry, binary} ->
                Runtime.load_and_run(entry, binary)

              _ ->
                span = BeamLang.Span.new(path, 0, 0)
                {:error, [BeamLang.Error.new(:type, "Missing entry module binary.", span)]}
            end

          {:error, %{message: message}} ->
            span = BeamLang.Span.new(path, 0, 0)
            {:error, [BeamLang.Error.new(:type, message, span)]}
        end

      {:error, errors} ->
        {:error, errors}
    end
  end
end
