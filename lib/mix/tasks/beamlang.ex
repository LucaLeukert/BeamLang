defmodule Mix.Tasks.Beamlang do
  use Mix.Task

  @shortdoc "Compile and run a BeamLang program"

  @moduledoc """
  Usage:
    mix beamlang path/to/file.bl [--print-tokens] [--print-ast] [--print-forms] [--emit-beam PATH] [--no-run]
  """

  @spec run([binary()]) :: :ok
  def run(args) do
    BeamLang.CLI.main(args)
  end
end
