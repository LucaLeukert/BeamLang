defmodule BeamLang.CLI do
  @moduledoc """
  Command-line interface for the BeamLang MVP compiler.
  """

  @spec main([binary()]) :: :ok
  def main(args) when is_list(args) do
    {opt_args, rest} = normalize_args(args)

    {opts, _rest_opts, _invalid} =
      OptionParser.parse(opt_args,
        strict: [
          print_tokens: :boolean,
          print_ast: :boolean,
          print_ast_pretty: :boolean,
          print_forms: :boolean,
          emit_beam: :string,
          no_run: :boolean
        ]
      )

    case rest do
      [path] when is_binary(path) ->
        if String.ends_with?(path, ".bl") do
          run_file(path, opts)
        else
          :ok
        end

      _ ->
        :ok
    end
  end

  @spec normalize_args([binary()]) :: {[binary()], [binary()]}
  defp normalize_args(args) do
    {opt_args, rest} = do_normalize(args, [], [])
    {Enum.reverse(opt_args), Enum.reverse(rest)}
  end

  @spec do_normalize([binary()], [binary()], [binary()]) :: {[binary()], [binary()]}
  defp do_normalize([], opt_acc, rest_acc), do: {opt_acc, rest_acc}

  @spec do_normalize([binary()], [binary()], [binary()]) :: {[binary()], [binary()]}
  defp do_normalize([arg, next | rest], opt_acc, rest_acc) do
    if String.starts_with?(arg, "--") do
      if arg == "--emit-beam" and is_binary(next) and not String.starts_with?(next, "--") do
        do_normalize(rest, [next, arg | opt_acc], rest_acc)
      else
        do_normalize([next | rest], [arg | opt_acc], rest_acc)
      end
    else
      do_normalize([next | rest], opt_acc, [arg | rest_acc])
    end
  end

  @spec do_normalize([binary()], [binary()], [binary()]) :: {[binary()], [binary()]}
  defp do_normalize([arg], opt_acc, rest_acc) do
    if String.starts_with?(arg, "--") do
      { [arg | opt_acc], rest_acc }
    else
      { opt_acc, [arg | rest_acc] }
    end
  end

  @spec run_file(binary(), keyword()) :: :ok
  defp run_file(path, opts) do
    source = File.read!(path)

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
              case BeamLang.Runtime.load_and_run(entry_module, elem(List.keyfind(modules, entry_module, 0), 1)) do
                {:ok, value} -> IO.puts(value)
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
