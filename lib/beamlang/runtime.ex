defmodule BeamLang.Runtime do
  @moduledoc """
  Compiles Erlang abstract forms to BEAM and executes main/0.
  """

  alias BeamLang.Codegen

  @spec compile_forms(list()) :: {:ok, atom(), binary()} | {:error, map()}
  def compile_forms(forms) do
    case :compile.forms(forms, [:return]) do
      {:ok, module, binary} -> {:ok, module, binary}
      {:ok, module, binary, _warnings} -> {:ok, module, binary}
      {:error, errors, warnings} -> {:error, %{message: "BEAM compile failed: #{inspect({errors, warnings})}"}}
    end
  end

  @spec load_and_run(atom(), binary()) :: {:ok, term()} | {:error, map()}
  def load_and_run(module, binary) do
    case :code.load_binary(module, ~c"beamlang_program.beam", binary) do
      {:module, ^module} -> {:ok, apply(module, :main, [])}
      {:error, reason} -> {:error, %{message: "Failed to load compiled module: #{inspect(reason)}"}}
    end
  end

  @spec compile_and_run(BeamLang.AST.t()) :: {:ok, term()} | {:error, map()}
  def compile_and_run(ast) do
    forms = Codegen.to_erlang_forms(ast)

    with {:ok, module, binary} <- compile_forms(forms),
         {:ok, value} <- load_and_run(module, binary) do
      {:ok, value}
    end
  end
end
