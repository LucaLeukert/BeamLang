defmodule BeamLang.CLITest do
  use ExUnit.Case, async: true

  test "parse_args supports --lsp flag" do
    {opts, rest} = BeamLang.CLI.parse_args(["--lsp"])
    assert opts[:lsp]
    assert rest == []
  end
end
