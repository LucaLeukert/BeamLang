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
      end
    end
  end

  @spec parse_command([binary()]) :: {:run | :compile | :format, [binary()]}
  def parse_command(["run" | rest]) when is_list(rest), do: {:run, rest}
  def parse_command(["compile" | rest]) when is_list(rest), do: {:compile, rest}
  def parse_command(["format" | rest]) when is_list(rest), do: {:format, rest}
  def parse_command(["fmt" | rest]) when is_list(rest), do: {:format, rest}
  def parse_command(args) when is_list(args), do: {:run, args}

  @spec parse_args([binary()], :run | :compile | :format) :: {keyword(), [binary()]}
  def parse_args(args, command \\ :run) when is_list(args) do
    case command do
      :compile -> parse_compile_args(args)
      :format -> parse_format_args(args)
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
    had_changes = Enum.reduce(files, false, fn file, changed ->
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
        if opts[:print_tokens] || opts[:print_ast] || opts[:print_ast_pretty] || opts[:print_forms] do
          case BeamLang.Lexer.tokenize(source, path) do
            {:ok, tokens} ->
              case BeamLang.Parser.parse(tokens) do
                {:ok, ast} ->
                  if opts[:print_tokens], do: IO.inspect(tokens, label: "TOKENS", limit: :infinity)
                  if opts[:print_ast], do: IO.inspect(ast, label: "AST")
                  if opts[:print_ast_pretty] do
                    IO.puts("AST (pretty):")
                    IO.puts(BeamLang.ASTPrinter.format(ast))
                  end
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
              case BeamLang.Runtime.load_and_run(entry_module, elem(List.keyfind(modules, entry_module, 0), 1), beamlang_args) do
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
          file == entry_path -> entry_source
          file == "<source>" -> entry_source
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
end
