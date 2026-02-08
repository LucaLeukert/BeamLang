defmodule BeamLang.LinterTest do
  use ExUnit.Case, async: true

  alias BeamLang.{Lexer, Linter, Parser}

  test "rewrites single-expression match branch blocks to fat arrows" do
    source = """
    fn main(args: [String]) -> number {
        let value = match (true) {
            case true -> { 1; },
            case false -> {
                println("x");
                return 0;
            }
        };
        return value;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    %{tokens: linted_tokens, diagnostics: diagnostics} = Linter.lint_tokens(tokens)

    fat_arrow_count =
      linted_tokens
      |> Enum.count(&(&1.type == :fat_arrow))

    assert fat_arrow_count == 1
    assert [%{rule: :single_expression_match_branch}] = diagnostics
    assert {:ok, _ast} = Parser.parse(linted_tokens)
  end

  test "lint/2 reports diagnostics with source location data" do
    source = """
    fn main(args: [String]) -> number {
        return match (true) {
            case true -> { 1; },
            case false => 0
        };
    }
    """

    assert {:ok, [diagnostic]} = Linter.lint(source, "sample.bl")
    assert diagnostic.line > 0
    assert diagnostic.col > 0
    assert diagnostic.message =~ "Use '=>'"
  end
end
