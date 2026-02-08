defmodule BeamLang.CLITest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

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

  test "parse_args supports --print-types flag" do
    {opts, rest} = BeamLang.CLI.parse_args(["--print-types", "file.bl"], :run)
    assert opts[:print_types]
    assert rest == ["file.bl"]
  end

  test "prints inferred and declared types for user code with --print-types" do
    path = Path.join(System.tmp_dir!(), "beamlang_cli_print_types.bl")

    source = """
    type User {
        name: String,
        age: number
    }

    fn main(args: [String]) -> number {
        let nums: [number] = [1, 2, 3];
        let first = nums->first()->unwrap(0);
        for (n in nums) {
            let x = n + 1;
            println(x);
        }
        return first;
    }
    """

    File.write!(path, source)

    output =
      capture_io(fn ->
        BeamLang.CLI.main(["--print-types", "--no-run", path])
      end)

    assert output =~ "TYPE DUMP:"
    assert output =~ "DECLARED TYPES:"
    assert output =~ "type User {"
    assert output =~ "FUNCTION TYPES:"
    assert output =~ "fn main(args: List<String>) -> number"
    assert output =~ "let nums: List<number>"
    assert output =~ "let first: number"
    assert output =~ "for n: number (in List<number>)"
  end
end
