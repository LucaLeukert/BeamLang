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
         {:ok, checked} <- Semantic.validate(ast) do
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
      {:error, %BeamLang.Error{} = error} -> {:error, [error]}
      {:error, errors} when is_list(errors) -> {:error, errors}
      {:error, other} ->
        span = BeamLang.Span.new(filename, 0, 0)
        {:error, [BeamLang.Error.new(:type, inspect(other), span)]}
    end
  end

  @spec run_source(binary()) :: {:ok, term()} | {:error, [BeamLang.Error.t()]}
  def run_source(source) when is_binary(source) do
    with {:ok, %{module: module, binary: binary}} <- compile_source(source),
         {:ok, value} <- Runtime.load_and_run(module, binary) do
      {:ok, value}
    end
  end

  @spec run_file(binary()) :: {:ok, term()} | {:error, [BeamLang.Error.t()]}
  def run_file(path) when is_binary(path) do
    case File.read(path) do
      {:ok, source} -> compile_source(source, path) |> run_compiled()
      {:error, reason} ->
        span = BeamLang.Span.new(path, 0, 0)
        {:error, [BeamLang.Error.new(:type, "Failed to read file: #{inspect(reason)}", span)]}
    end
  end

  @spec run_compiled({:ok, map()} | {:error, [BeamLang.Error.t()]}) ::
          {:ok, term()} | {:error, [BeamLang.Error.t()]}
  defp run_compiled({:ok, %{module: module, binary: binary}}) do
    Runtime.load_and_run(module, binary)
  end

  defp run_compiled({:error, _} = error), do: error
end
