defmodule BeamLang.CLI do
  @moduledoc """
  Command-line interface for the BeamLang MVP compiler.
  """

  @spec main([binary()]) :: :ok
  def main(args) when is_list(args) do
    {command, args} = parse_command(args)
    {opts, rest} = parse_args(args, command)

    if opts[:lsp] do
      if opts[:lsp_debug] do
        System.put_env("BEAMLANG_LSP_DEBUG", "1")
      end

      BeamLang.LSP.Server.start()
      :ok
    else
      case command do
        :run -> run_command(rest, opts)
        :compile -> compile_command(rest, opts)
        :format -> format_command(rest, opts)
        :lint -> lint_command(rest)
      end
    end
  end

  @spec parse_command([binary()]) :: {:run | :compile | :format | :lint, [binary()]}
  def parse_command(["run" | rest]) when is_list(rest), do: {:run, rest}
  def parse_command(["compile" | rest]) when is_list(rest), do: {:compile, rest}
  def parse_command(["format" | rest]) when is_list(rest), do: {:format, rest}
  def parse_command(["fmt" | rest]) when is_list(rest), do: {:format, rest}
  def parse_command(["lint" | rest]) when is_list(rest), do: {:lint, rest}
  def parse_command(args) when is_list(args), do: {:run, args}

  @spec parse_args([binary()], :run | :compile | :format | :lint) :: {keyword(), [binary()]}
  def parse_args(args, command \\ :run) when is_list(args) do
    case command do
      :compile -> parse_compile_args(args)
      :format -> parse_format_args(args)
      :lint -> {[], args}
      :run -> parse_run_args(args)
    end
  end

  defp parse_run_args(args) when is_list(args) do
    {opt_args, rest} = normalize_args(args)

    {opts, _rest_opts, _invalid} =
      OptionParser.parse(opt_args,
        strict: [
          print_tokens: :boolean,
          print_ast: :boolean,
          print_ast_pretty: :boolean,
          print_types: :boolean,
          print_forms: :boolean,
          emit_beam: :string,
          no_run: :boolean,
          lsp: :boolean,
          lsp_debug: :boolean
        ]
      )

    {opts, rest}
  end

  defp parse_compile_args(args) when is_list(args) do
    case args do
      [path | opt_args] ->
        {opts, _rest, _invalid} =
          OptionParser.parse(opt_args,
            strict: [
              print_tokens: :boolean,
              print_ast: :boolean,
              print_ast_pretty: :boolean,
              print_types: :boolean,
              print_forms: :boolean,
              emit_beam: :string,
              lsp: :boolean,
              lsp_debug: :boolean
            ]
          )

        {opts, [path]}

      _ ->
        {[], []}
    end
  end

  @spec normalize_args([binary()]) :: {[binary()], [binary()]}
  defp normalize_args(args) do
    # Find the .bl file - everything before it is CLI options, everything after is program args
    {opt_args, rest} = do_normalize_with_bl_split(args, [])
    {Enum.reverse(opt_args), rest}
  end

  # Split args at the .bl file: opts before, program args after
  defp do_normalize_with_bl_split([], opt_acc), do: {opt_acc, []}

  defp do_normalize_with_bl_split([arg | rest], opt_acc) do
    if String.ends_with?(arg, ".bl") do
      # Found the .bl file, everything after is program args
      {opt_acc, [arg | rest]}
    else
      # Before the .bl file, treat everything as CLI args for OptionParser
      do_normalize_with_bl_split(rest, [arg | opt_acc])
    end
  end

  defp parse_format_args(args) when is_list(args) do
    {opts, rest, _invalid} =
      OptionParser.parse(args,
        strict: [
          write: :boolean,
          check: :boolean
        ],
        aliases: [w: :write, c: :check]
      )

    {opts, rest}
  end

  defp format_command(rest, opts) do
    files =
      case rest do
        [] ->
          # Format all .bl files in the current directory recursively
          Path.wildcard("**/*.bl")

        paths ->
          Enum.flat_map(paths, fn path ->
            if File.dir?(path) do
              Path.wildcard(Path.join(path, "**/*.bl"))
            else
              [path]
            end
          end)
      end

    if files == [] do
      IO.puts(:stderr, "No .bl files found")
      System.halt(1)
    end

    check_mode = opts[:check] || false
    write_mode = opts[:write] || false

    had_changes =
      Enum.reduce(files, false, fn file, changed ->
        format_single_file(file, check_mode, write_mode) or changed
      end)

    if check_mode and had_changes do
      System.halt(1)
    end

    :ok
  end

  defp format_single_file(path, check_mode, write_mode) do
    case BeamLang.Formatter.format_file(path) do
      {:ok, formatted} ->
        original = File.read!(path)

        if formatted == original do
          false
        else
          if check_mode do
            IO.puts("#{path} needs formatting")
            true
          else
            if write_mode do
              File.write!(path, formatted)
              IO.puts("Formatted #{path}")
            else
              IO.write(formatted)
            end

            true
          end
        end

      {:error, message} ->
        IO.puts(:stderr, "Error formatting #{path}: #{message}")
        false
    end
  end

  defp lint_command(rest) do
    files =
      case rest do
        [] ->
          Path.wildcard("**/*.bl")

        paths ->
          Enum.flat_map(paths, fn path ->
            if File.dir?(path) do
              Path.wildcard(Path.join(path, "**/*.bl"))
            else
              [path]
            end
          end)
      end

    if files == [] do
      IO.puts(:stderr, "No .bl files found")
      System.halt(1)
    end

    findings =
      Enum.reduce(files, 0, fn file, acc ->
        acc + lint_single_file(file)
      end)

    if findings > 0 do
      System.halt(1)
    end

    :ok
  end

  defp lint_single_file(path) do
    case File.read(path) do
      {:ok, source} ->
        case BeamLang.Linter.lint(source, path) do
          {:ok, diagnostics} ->
            Enum.each(diagnostics, fn diagnostic ->
              IO.puts("#{path}:#{diagnostic.line}:#{diagnostic.col}: #{diagnostic.message}")
            end)

            length(diagnostics)

          {:error, reason} ->
            IO.puts(:stderr, "Error linting #{path}: #{reason}")
            0
        end

      {:error, reason} ->
        IO.puts(:stderr, "Error reading #{path}: #{inspect(reason)}")
        0
    end
  end

  defp run_command(rest, opts) do
    case rest do
      [path | program_args] when is_binary(path) ->
        if String.ends_with?(path, ".bl") do
          run_file(path, opts, program_args)
        else
          :ok
        end

      _ ->
        :ok
    end
  end

  defp compile_command(rest, opts) do
    case rest do
      [path] when is_binary(path) ->
        if String.ends_with?(path, ".bl") do
          output_path = Path.rootname(path)

          opts =
            opts
            |> Keyword.put(:compile, output_path)
            |> Keyword.put(:no_run, true)

          run_file(path, opts, [])
        else
          :ok
        end

      _ ->
        :ok
    end
  end

  @spec run_file(binary(), keyword(), [binary()]) :: :ok
  defp run_file(path, opts, program_args) do
    source = File.read!(path)
    # Convert Elixir strings to BeamLang strings (charlists)
    beamlang_args = Enum.map(program_args, &String.to_charlist/1)

    case BeamLang.compile_file(path) do
      {:ok, %{entry: entry_module, modules: modules}} ->
        if opts[:print_tokens] || opts[:print_ast] || opts[:print_ast_pretty] ||
             opts[:print_types] || opts[:print_forms] do
          case BeamLang.Lexer.tokenize(source, path) do
            {:ok, tokens} ->
              case BeamLang.Parser.parse(tokens) do
                {:ok, ast} ->
                  if opts[:print_tokens],
                    do: IO.inspect(tokens, label: "TOKENS", limit: :infinity)

                  if opts[:print_ast], do: IO.inspect(ast, label: "AST")

                  if opts[:print_ast_pretty] do
                    IO.puts("AST (pretty):")
                    IO.puts(BeamLang.ASTPrinter.format(ast))
                  end

                  if opts[:print_types], do: print_types(source, path)

                  if opts[:print_forms] do
                    forms = BeamLang.Codegen.to_erlang_forms(ast)
                    IO.inspect(forms, label: "ERLANG_FORMS", limit: :infinity)
                  end

                _ ->
                  :ok
              end

            _ ->
              :ok
          end
        end

        if is_binary(opts[:emit_beam]) do
          case List.keyfind(modules, entry_module, 0) do
            {^entry_module, binary} -> File.write!(opts[:emit_beam], binary)
            _ -> :ok
          end
        end

        if is_binary(opts[:compile]) do
          case emit_standalone_binary(opts[:compile], entry_module, modules) do
            :ok ->
              :ok

            {:error, message} ->
              IO.puts(:stderr, "Error: #{message}")
              System.halt(1)
          end
        end

        if opts[:no_run] do
          :ok
        else
          case BeamLang.Runtime.load_modules(modules) do
            :ok ->
              case BeamLang.Runtime.load_and_run(
                     entry_module,
                     elem(List.keyfind(modules, entry_module, 0), 1),
                     beamlang_args
                   ) do
                {:ok, exit_code} when is_integer(exit_code) ->
                  System.halt(exit_code)

                {:ok, _value} ->
                  System.halt(0)

                {:error, %{message: message}} ->
                  IO.puts(:stderr, "Error: #{message}")
                  System.halt(1)
              end

            {:error, %{message: message}} ->
              IO.puts(:stderr, "Error: #{message}")
              System.halt(1)
          end
        end

      {:error, errors} when is_list(errors) ->
        IO.puts(:stderr, format_errors(errors, source, path))
        System.halt(1)
    end
  end

  defp format_errors(errors, entry_source, entry_path) do
    errors
    |> Enum.group_by(fn %BeamLang.Error{span: span} -> span.file_id end)
    |> Enum.map(fn {file, errs} ->
      source =
        cond do
          file == entry_path ->
            entry_source

          file == "<source>" ->
            entry_source

          true ->
            case File.read(file) do
              {:ok, contents} -> contents
              _ -> entry_source
            end
        end

      BeamLang.Errors.format(errs, source)
    end)
    |> Enum.join("\n")
  end

  defp emit_standalone_binary(output_path, entry_module, modules) do
    with {:ok, runtime_binary} <- get_module_binary(BeamLang.Runtime),
         :ok <- create_elixir_script(output_path, entry_module, runtime_binary, modules) do
      :ok
    else
      {:error, message} -> {:error, message}
    end
  end

  defp get_module_binary(module) do
    case :code.get_object_code(module) do
      {^module, binary, _} -> {:ok, binary}
      :error -> {:error, "Failed to load #{inspect(module)} bytecode."}
    end
  end

  defp create_elixir_script(output_path, entry_module, runtime_binary, modules) do
    script = build_elixir_script(entry_module, runtime_binary, modules)
    File.write!(output_path, script)
    File.chmod!(output_path, 0o755)
    :ok
  end

  defp build_elixir_script(entry_module, runtime_binary, modules) do
    runtime_b64 = Base.encode64(runtime_binary)

    modules_b64 =
      modules
      |> Enum.map(fn {module, binary} ->
        {module, Base.encode64(binary)}
      end)

    runtime_chunks = chunk_string(runtime_b64, 1024)

    runtime_literal =
      runtime_chunks
      |> Enum.map(&"  #{inspect(&1)}")
      |> Enum.join(",\n")

    modules_literal =
      modules_b64
      |> Enum.map(fn {module, b64} ->
        chunks =
          b64
          |> chunk_string(1024)
          |> Enum.map(&"    #{inspect(&1)}")
          |> Enum.join(",\n")

        "  {#{inspect(module)}, [\n#{chunks}\n  ]}"
      end)
      |> Enum.join(",\n")

    """
    #!/usr/bin/env elixir
    # BeamLang standalone binary

    entry_module = #{inspect(entry_module)}
    runtime_b64 =
    [
    #{runtime_literal}
    ]
    |> Enum.join("")

    modules = [
    #{modules_literal}
    ]

    defmodule BeamLangStandalone do
      def run(entry_module, runtime_b64, modules, args) do
        load_module(BeamLang.Runtime, runtime_b64, "beamlang_runtime.beam")

        Enum.each(modules, fn {mod, chunks} ->
          load_module(mod, Enum.join(chunks, ""), "beamlang_module.beam")
        end)

        Process.put(:beamlang_module, entry_module)
        beamlang_args = Enum.map(args, &String.to_charlist/1)
        beamlang_list = BeamLang.Runtime.wrap_as_beamlang_list(beamlang_args, entry_module)
        result = apply(entry_module, :main, [beamlang_list])

        case result do
          exit_code when is_integer(exit_code) -> System.halt(exit_code)
          _ -> System.halt(0)
        end
      end

      defp load_module(module, b64, filename) do
        binary = Base.decode64!(b64)
        case :code.load_binary(module, String.to_charlist(filename), binary) do
          {:module, ^module} -> :ok
          {:error, reason} -> raise "Failed to load \#{module}: \#{inspect(reason)}"
        end
      end
    end

    BeamLangStandalone.run(entry_module, runtime_b64, modules, System.argv())
    """
  end

  defp chunk_string(str, size) when is_binary(str) and is_integer(size) and size > 0 do
    if byte_size(str) <= size do
      [str]
    else
      {head, tail} = String.split_at(str, size)
      [head | chunk_string(tail, size)]
    end
  end

  defp print_types(source, path) do
    case BeamLang.analyze_source(source, path) do
      {:ok, %{ast: ast}} ->
        print_type_dump(ast, path)

      _ ->
        :ok
    end
  end

  defp print_type_dump({:program, info}, path) do
    types = Map.get(info, :types, [])
    enums = Map.get(info, :enums, [])
    errors = Map.get(info, :errors, [])
    functions = Map.get(info, :functions, [])

    IO.puts("TYPE DUMP:")

    print_declared_types(types, errors, enums, path)
    print_function_types(functions, path)
  end

  defp print_declared_types(types, errors, enums, path) do
    docs =
      (Enum.map(types, &format_type_def(&1, path)) ++
         Enum.map(errors, &format_error_def(&1, path)) ++
         Enum.map(enums, &format_enum_def(&1, path)))
      |> Enum.reject(&is_nil/1)

    IO.puts("DECLARED TYPES:")

    case docs do
      [] -> IO.puts("(none)")
      _ -> IO.puts(Enum.join(docs, "\n\n"))
    end
  end

  defp print_function_types(functions, path) do
    function_docs =
      functions
      |> Enum.map(&format_function_type_dump(&1, path))
      |> Enum.reject(&is_nil/1)

    IO.puts("\nFUNCTION TYPES:")

    case function_docs do
      [] ->
        IO.puts("(none)")

      _ ->
        IO.puts(Enum.join(function_docs, "\n\n"))
    end
  end

  defp format_function_type_dump(
         {:function,
          %{
            name: name,
            type_params: type_params,
            params: params,
            return_type: return_type,
            body: body,
            span: span
          }},
         path
       ) do
    if span.file_id != path do
      nil
    else
      header =
        "fn #{name}#{format_type_params(type_params)}(#{format_params(params)}) -> #{format_type(return_type)}"

      bindings = collect_local_bindings(body, path)
      expr_types = collect_expression_types(body, path)

      body_lines =
        (Enum.map(bindings, &format_binding/1) ++ Enum.map(expr_types, &format_expr_type/1))
        |> case do
          [] -> ["  (no local typed bindings/expressions)"]
          lines -> lines
        end

      Enum.join([header | body_lines], "\n")
    end
  end

  defp format_function_type_dump(_other, _path), do: nil

  defp format_params(params) do
    params
    |> Enum.map(fn
      %{name: name, type: type} -> "#{name}: #{format_type(type)}"
      %{pattern: _pattern, type: type} -> "_pattern: #{format_type(type)}"
    end)
    |> Enum.join(", ")
  end

  defp format_binding({:let, name, type, span}),
    do: "  let #{name}: #{format_type(type)} @#{span.start}"

  defp format_binding({:for_item, name, item_type, collection_type, span}),
    do:
      "  for #{name}: #{format_type(item_type)} (in #{format_type(collection_type)}) @#{span.start}"

  defp format_expr_type({label, type, span}),
    do: "  expr #{label}: #{format_type(type)} @#{span.start}"

  defp collect_local_bindings(nil, _path), do: []

  defp collect_local_bindings({:block, %{stmts: stmts}}, path),
    do: collect_stmt_bindings(stmts, path)

  defp collect_stmt_bindings(stmts, path) do
    Enum.flat_map(stmts, &collect_stmt_binding(&1, path))
  end

  defp collect_stmt_binding({:let, %{name: name, inferred_type: type, span: span}}, path) do
    if span.file_id == path, do: [{:let, name, type || :any, span}], else: []
  end

  defp collect_stmt_binding(
         {:for,
          %{
            name: name,
            item_type: item_type,
            collection_type: collection_type,
            body: body,
            span: span
          }},
         path
       ) do
    current =
      if span.file_id == path do
        [{:for_item, name, item_type || :any, collection_type || :any, span}]
      else
        []
      end

    current ++ collect_local_bindings(body, path)
  end

  defp collect_stmt_binding({:if_stmt, %{then_block: then_block, else_branch: else_branch}}, path) do
    collect_local_bindings(then_block, path) ++ collect_else_bindings(else_branch, path)
  end

  defp collect_stmt_binding({:while, %{body: body}}, path), do: collect_local_bindings(body, path)
  defp collect_stmt_binding({:loop, %{body: body}}, path), do: collect_local_bindings(body, path)

  defp collect_stmt_binding({:guard, %{else_block: else_block}}, path),
    do: collect_local_bindings(else_block, path)

  defp collect_stmt_binding(_stmt, _path), do: []

  defp collect_else_bindings(nil, _path), do: []

  defp collect_else_bindings({:else_block, %{block: block}}, path),
    do: collect_local_bindings(block, path)

  defp collect_else_bindings({:else_if, %{if: if_stmt}}, path),
    do: collect_stmt_binding(if_stmt, path)

  defp collect_expression_types(nil, _path), do: []

  defp collect_expression_types({:block, %{stmts: stmts}}, path),
    do: Enum.flat_map(stmts, &collect_stmt_expr_types(&1, path))

  defp collect_stmt_expr_types({:let, %{expr: expr}}, path), do: collect_expr_types(expr, path)
  defp collect_stmt_expr_types({:expr, %{expr: expr}}, path), do: collect_expr_types(expr, path)

  defp collect_stmt_expr_types({:return, %{expr: expr}}, path) when not is_nil(expr),
    do: collect_expr_types(expr, path)

  defp collect_stmt_expr_types({:assign, %{expr: expr}}, path), do: collect_expr_types(expr, path)

  defp collect_stmt_expr_types({:compound_assign, %{expr: expr}}, path),
    do: collect_expr_types(expr, path)

  defp collect_stmt_expr_types({:let_destruct, %{expr: expr}}, path),
    do: collect_expr_types(expr, path)

  defp collect_stmt_expr_types(
         {:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}},
         path
       ) do
    collect_expr_types(cond, path) ++
      collect_expression_types(then_block, path) ++ collect_else_expr_types(else_branch, path)
  end

  defp collect_stmt_expr_types({:while, %{cond: cond, body: body}}, path),
    do: collect_expr_types(cond, path) ++ collect_expression_types(body, path)

  defp collect_stmt_expr_types({:loop, %{body: body}}, path),
    do: collect_expression_types(body, path)

  defp collect_stmt_expr_types({:for, %{collection: collection, body: body}}, path),
    do: collect_expr_types(collection, path) ++ collect_expression_types(body, path)

  defp collect_stmt_expr_types({:guard, %{cond: cond, else_block: else_block}}, path),
    do: collect_expr_types(cond, path) ++ collect_expression_types(else_block, path)

  defp collect_stmt_expr_types(_stmt, _path), do: []

  defp collect_else_expr_types(nil, _path), do: []

  defp collect_else_expr_types({:else_block, %{block: block}}, path),
    do: collect_expression_types(block, path)

  defp collect_else_expr_types({:else_if, %{if: if_stmt}}, path),
    do: collect_stmt_expr_types(if_stmt, path)

  defp collect_expr_types(expr, path) do
    own = maybe_collect_expr_type(expr, path)
    own ++ nested_expr_types(expr, path)
  end

  defp maybe_collect_expr_type({:list_literal, %{type: type, span: span}}, path),
    do: if(span.file_id == path, do: [{"list_literal", type || :any, span}], else: [])

  defp maybe_collect_expr_type({:opt_some, %{type: type, span: span}}, path),
    do: if(span.file_id == path, do: [{"optional_some", type || :any, span}], else: [])

  defp maybe_collect_expr_type({:opt_none, %{type: type, span: span}}, path),
    do: if(span.file_id == path, do: [{"optional_none", type || :any, span}], else: [])

  defp maybe_collect_expr_type({:res_ok, %{type: type, span: span}}, path),
    do: if(span.file_id == path, do: [{"result_ok", type || :any, span}], else: [])

  defp maybe_collect_expr_type({:res_err, %{type: type, span: span}}, path),
    do: if(span.file_id == path, do: [{"result_err", type || :any, span}], else: [])

  defp maybe_collect_expr_type({:struct, %{type: type, span: span}}, path),
    do: if(span.file_id == path and not is_nil(type), do: [{"struct", type, span}], else: [])

  defp maybe_collect_expr_type({:method_call, %{target_type: type, span: span}}, path),
    do:
      if(span.file_id == path and not is_nil(type), do: [{"method_target", type, span}], else: [])

  defp maybe_collect_expr_type(
         {:binary, %{operator_info: %{result_type: type}, span: span}},
         path
       ),
       do: if(span.file_id == path, do: [{"binary_result", type, span}], else: [])

  defp maybe_collect_expr_type(_expr, _path), do: []

  defp nested_expr_types({:call, %{args: args}}, path),
    do: Enum.flat_map(args, &collect_expr_types(&1, path))

  defp nested_expr_types({:method_call, %{target: target, args: args}}, path),
    do: collect_expr_types(target, path) ++ Enum.flat_map(args, &collect_expr_types(&1, path))

  defp nested_expr_types({:field, %{target: target}}, path), do: collect_expr_types(target, path)

  defp nested_expr_types({:binary, %{left: left, right: right}}, path),
    do: collect_expr_types(left, path) ++ collect_expr_types(right, path)

  defp nested_expr_types(
         {:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch}},
         path
       ),
       do:
         collect_expr_types(cond, path) ++
           collect_expression_types(then_block, path) ++
           collect_else_expr_types(else_branch, path)

  defp nested_expr_types({:block_expr, %{block: block}}, path),
    do: collect_expression_types(block, path)

  defp nested_expr_types({:match, %{expr: expr, cases: cases}}, path),
    do:
      collect_expr_types(expr, path) ++
        Enum.flat_map(cases, fn %{guard: guard, body: body} ->
          if(guard == nil, do: [], else: collect_expr_types(guard, path)) ++
            collect_expr_types(body, path)
        end)

  defp nested_expr_types({:struct, %{fields: fields}}, path),
    do: Enum.flat_map(fields, fn %{expr: expr} -> collect_expr_types(expr, path) end)

  defp nested_expr_types({:tuple, %{elements: elements}}, path),
    do: Enum.flat_map(elements, &collect_expr_types(&1, path))

  defp nested_expr_types({:list_literal, %{elements: elements}}, path),
    do: Enum.flat_map(elements, &collect_expr_types(&1, path))

  defp nested_expr_types({:opt_some, %{expr: expr}}, path), do: collect_expr_types(expr, path)
  defp nested_expr_types({:res_ok, %{expr: expr}}, path), do: collect_expr_types(expr, path)
  defp nested_expr_types({:res_err, %{expr: expr}}, path), do: collect_expr_types(expr, path)
  defp nested_expr_types({:try_expr, %{expr: expr}}, path), do: collect_expr_types(expr, path)

  defp nested_expr_types({:enum_variant, %{fields: fields}}, path) when is_list(fields) do
    Enum.flat_map(fields, fn
      {_, value} -> collect_expr_types(value, path)
      %{expr: expr} -> collect_expr_types(expr, path)
      _ -> []
    end)
  end

  defp nested_expr_types(_expr, _path), do: []

  defp format_type_def(
         {:type_def, %{name: name, params: params, fields: fields, span: span}},
         path
       ) do
    if span.file_id != path do
      nil
    else
      header = "type #{name}#{format_type_params(params)} {"
      body = Enum.map(fields, &format_field/1) |> Enum.join("\n")
      [header, body, "}"] |> Enum.join("\n")
    end
  end

  defp format_error_def({:error_def, %{name: name, fields: fields, span: span}}, path) do
    if span.file_id != path do
      nil
    else
      header = "error #{name} {"
      body = Enum.map(fields, &format_field/1) |> Enum.join("\n")
      [header, body, "}"] |> Enum.join("\n")
    end
  end

  defp format_enum_def(
         {:enum_def, %{name: name, params: params, variants: variants, span: span}},
         path
       ) do
    if span.file_id != path do
      nil
    else
      header = "enum #{name}#{format_type_params(params)} {"
      body = Enum.map(variants, &format_variant/1) |> Enum.join("\n")
      [header, body, "}"] |> Enum.join("\n")
    end
  end

  defp format_field(%{name: name, type: type, internal: internal}) do
    visibility = if internal, do: "internal ", else: ""
    "  #{visibility}#{name}: #{format_type(type)}"
  end

  defp format_variant(%{name: name, fields: fields}) when fields == nil or fields == [] do
    "  #{name}"
  end

  defp format_variant(%{name: name, fields: fields}) do
    formatted_fields =
      fields
      |> Enum.map(fn %{name: field_name, type: field_type} ->
        "#{field_name}: #{format_type(field_type)}"
      end)
      |> Enum.join(", ")

    "  #{name} { #{formatted_fields} }"
  end

  defp format_type_params([]), do: ""
  defp format_type_params(params), do: "<" <> Enum.join(params, ", ") <> ">"

  defp format_type(:number), do: "number"
  defp format_type(:String), do: "String"
  defp format_type(:char), do: "char"
  defp format_type(:bool), do: "bool"
  defp format_type(:void), do: "void"
  defp format_type(:any), do: "any"
  defp format_type({:named, name}), do: name
  defp format_type({:optional, inner}), do: "#{format_type(inner)}?"
  defp format_type({:result, ok, err}), do: "#{format_type(ok)}!#{format_type(err)}"
  defp format_type({:tuple, items}), do: "(" <> Enum.map_join(items, ", ", &format_type/1) <> ")"
  defp format_type({:type_var, name}), do: name

  defp format_type({:generic, base, args}) do
    "#{format_type(base)}<#{Enum.map_join(args, ", ", &format_type/1)}>"
  end

  defp format_type({:fn, params, return_type}) do
    "fn(#{Enum.map_join(params, ", ", &format_type/1)}) -> #{format_type(return_type)}"
  end
end
