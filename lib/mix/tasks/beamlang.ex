defmodule Mix.Tasks.Beamlang do
  use Mix.Task

  @shortdoc "Compile and run a BeamLang program"

  @moduledoc """
  Usage:
    mix beamlang path/to/file.bl [--print-tokens] [--print-ast] [--print-forms] [--emit-beam PATH] [--no-run]
    mix beamlang --lsp
  """

  @spec run([binary()]) :: :ok
  def run(args) do
    if Enum.member?(args, "--lsp") do
      Mix.shell(Mix.Shell.Quiet)
      Logger.configure(level: :error)
    end

    BeamLang.CLI.main(args)
  end
end
