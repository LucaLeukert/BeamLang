defmodule BeamLang.CLI do
  @moduledoc """
  Command-line interface for the BeamLang MVP compiler.
  """

  @spec main([binary()]) :: :ok
  def main(args) when is_list(args) do
    {opts, rest} = parse_args(args)

    if opts[:lsp] do
      if opts[:lsp_debug] do
        System.put_env("BEAMLANG_LSP_DEBUG", "1")
      end

      BeamLang.LSP.Server.start()
      :ok
    else
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
  end

  @spec parse_args([binary()]) :: {keyword(), [binary()]}
  def parse_args(args) when is_list(args) do
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

  @spec normalize_args([binary()]) :: {[binary()], [binary()]}
  defp normalize_args(args) do
    # Find the .bl file - everything before it is CLI options, everything after is program args
    {opt_args, rest} = do_normalize_with_bl_split(args, [], [])
    {Enum.reverse(opt_args), rest}
  end

  # Split args at the .bl file: opts before, program args after
  defp do_normalize_with_bl_split([], opt_acc, _rest_acc), do: {opt_acc, []}
  defp do_normalize_with_bl_split([arg | rest], opt_acc, rest_acc) do
    if String.ends_with?(arg, ".bl") do
      # Found the .bl file, everything after is program args
      {opt_acc, [arg | rest]}
    else
      # Before the .bl file, check if it's a CLI option
      if String.starts_with?(arg, "--") do
        do_normalize_with_bl_split(rest, [arg | opt_acc], rest_acc)
      else
        do_normalize_with_bl_split(rest, opt_acc, [arg | rest_acc])
      end
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

        unless opts[:no_run] do
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
end
