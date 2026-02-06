defmodule BeamLang.CLITest do
  use ExUnit.Case, async: true

  test "parse_args supports --lsp flag" do
    {opts, rest} = BeamLang.CLI.parse_args(["--lsp"], :run)
    assert opts[:lsp]
    assert rest == []
  end

  test "parse_args supports --lsp-debug flag" do
    {opts, rest} = BeamLang.CLI.parse_args(["--lsp", "--lsp-debug"], :run)
    assert opts[:lsp]
    assert opts[:lsp_debug]
    assert rest == []
  end

  test "parse_command recognizes compile subcommand" do
    {command, args} = BeamLang.CLI.parse_command(["compile", "file.bl", "--print-ast"])
    assert command == :compile
    assert args == ["file.bl", "--print-ast"]
  end

  test "parse_args supports compile subcommand ordering" do
    {opts, rest} = BeamLang.CLI.parse_args(["file.bl", "--print-ast"], :compile)
    assert opts[:print_ast]
    assert rest == ["file.bl"]
  end
end
