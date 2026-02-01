defmodule BeamLang.LexerTest do
  use ExUnit.Case, async: true

  alias BeamLang.Lexer

  test "tokenizes minimal function" do
    source = """
    fn main() -> number {
        return 42;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)

    types = Enum.map(tokens, & &1.type)
    values = Enum.map(tokens, & &1.value)

    assert types == [
             :fn,
             :identifier,
             :lparen,
             :rparen,
             :arrow,
             :type,
             :lbrace,
             :return,
             :integer,
             :semicolon,
             :rbrace
           ]

    assert values == [
             "fn",
             "main",
             "(",
             ")",
             "->",
             "number",
             "{",
             "return",
             42,
             ";",
             "}"
           ]
  end

  test "tokenizes bool literals" do
    source = """
    fn main() -> bool {
        return false;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)

    types = Enum.map(tokens, & &1.type)
    values = Enum.map(tokens, & &1.value)

    assert types == [
             :fn,
             :identifier,
             :lparen,
             :rparen,
             :arrow,
             :type,
             :lbrace,
             :return,
             :bool,
             :semicolon,
             :rbrace
           ]

    assert values == [
             "fn",
             "main",
             "(",
             ")",
             "->",
             "bool",
             "{",
             "return",
             "false",
             ";",
             "}"
           ]
  end

  test "tokenizes let and mut" do
    source = """
    fn main() -> i32 {
        let mut value = 1;
        return value;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    types = Enum.map(tokens, & &1.type)

    assert :let in types
    assert :mut in types
    assert :equals in types
  end

  test "tokenizes match and case" do
    source = """
    fn main() -> i32 {
        return match (1) { case 1 if 1 != 2 => 2, case _ => 0 };
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    types = Enum.map(tokens, & &1.type)

    assert :match in types
    assert :case in types
    assert :fat_arrow in types
    assert :neq in types
    assert :if_kw in types
  end

  test "tokenizes loop keywords" do
    source = """
    fn main() -> i32 {
        if (true) { return 1; } else { return 2; }
        while (false) { break; }
        loop { break; }
        for (item in "hi"->chars()) { break; }
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    types = Enum.map(tokens, & &1.type)

    assert :if_kw in types
    assert :else_kw in types
    assert :while_kw in types
    assert :loop_kw in types
    assert :for_kw in types
    assert :in_kw in types
    assert :break_kw in types
  end

  test "ignores single-line comments" do
    source = """
    // This is a comment
    fn main(args: [String]) -> number {
        // Another comment
        return 42; // inline comment
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    types = Enum.map(tokens, & &1.type)

    # Comments should be ignored, so we should only see the code tokens
    assert :fn in types
    assert :return in types
    refute Enum.any?(tokens, fn t -> String.contains?(to_string(t.value), "comment") end)
  end

  test "ignores block comments" do
    source = """
    /** This is a block comment **/
    fn main(args: [String]) -> number {
        /**
         * Multi-line
         * block comment
         **/
        return 42;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    types = Enum.map(tokens, & &1.type)

    assert :fn in types
    assert :return in types
    refute Enum.any?(tokens, fn t -> String.contains?(to_string(t.value), "comment") end)
  end

  test "reports unterminated block comment" do
    source = """
    /** Unterminated comment
    fn main(args: [String]) -> number {
        return 42;
    }
    """

    assert {:error, %BeamLang.Error{message: "Unterminated block comment."}} = Lexer.tokenize(source)
  end
end
