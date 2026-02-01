defmodule BeamLang.ParserMatchTest do
  use ExUnit.Case, async: true

  alias BeamLang.{Lexer, Parser}

  test "parses let match expression" do
    source = """
    fn main(args: [String]) -> number {
        let code = match (args->first()) {
            case?some filename => 1,
            case?none => 2
        };
        return code;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [stmt, _]}}}} = func
    assert {:let, %{expr: {:match, _}}} = stmt
  end

  test "parses match expression with boolean cases" do
    source = """
    fn main(args: [String]) -> number {
        return match (true) {
            case true => 1,
            case false => 0
        };
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [stmt]}}}} = func
    assert {:return, %{expr: {:match, _}}} = stmt
  end
end
