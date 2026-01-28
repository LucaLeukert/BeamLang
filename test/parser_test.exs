defmodule BeamLang.ParserTest do
  use ExUnit.Case, async: true

  alias BeamLang.{Lexer, Parser}

  test "parses minimal function" do
    source = """
    fn main() -> i32 {
        return 42;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{name: "main", return_type: :i32, body: {:block, %{stmts: [stmt]}}}} = func
    assert {:return, %{expr: {:integer, %{value: 42}}}} = stmt
  end

  test "parses bool return" do
    source = """
    fn main() -> bool {
        return true;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{name: "main", return_type: :bool, body: {:block, %{stmts: [stmt]}}}} = func
    assert {:return, %{expr: {:bool, %{value: true}}}} = stmt
  end

  test "parses function call return" do
    source = """
    fn main() -> i32 {
        return helper(1);
    }

    fn helper(value: i32) -> i32 {
        return 1;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [main, helper]}} = ast
    assert {:function, %{name: "main", return_type: :i32, body: {:block, %{stmts: [stmt1]}}}} = main
    assert {:return, %{expr: {:call, %{name: "helper", args: [arg]}}}} = stmt1
    assert {:integer, %{value: 1}} = arg
    assert {:function, %{name: "helper", return_type: :i32, params: [_], body: {:block, %{stmts: [stmt2]}}}} = helper
    assert {:return, %{expr: {:integer, %{value: 1}}}} = stmt2
  end

  test "parses call statement before return" do
    source = """
    fn main() -> i32 {
        helper(1);
        return 1;
    }

    fn helper(value: i32) -> i32 {
        return 2;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [main, helper]}} = ast
    assert {:function, %{name: "main", return_type: :i32, body: {:block, %{stmts: [s1, s2]}}}} = main
    assert {:expr, %{expr: {:call, %{name: "helper", args: [arg]}}}} = s1
    assert {:integer, %{value: 1}} = arg
    assert {:return, %{expr: {:integer, %{value: 1}}}} = s2
    assert {:function, %{name: "helper", return_type: :i32, params: [_], body: {:block, %{stmts: [h1]}}}} = helper
    assert {:return, %{expr: {:integer, %{value: 2}}}} = h1
  end

  test "parses let and identifier return" do
    source = """
    fn main() -> i32 {
        let value = 3;
        return value;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{name: "main", return_type: :i32, body: {:block, %{stmts: [s1, s2]}}}} = func
    assert {:let, %{name: "value", mutable: false, expr: {:integer, %{value: 3}}}} = s1
    assert {:return, %{expr: {:identifier, %{name: "value"}}}} = s2
  end

  test "parses assignment statement" do
    source = """
    fn main() -> i32 {
        let mut value = 1;
        value = 2;
        return value;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [s1, s2, s3]}}}} = func
    assert {:let, %{name: "value", mutable: true}} = s1
    assert {:assign, %{target: {:identifier, %{name: "value"}}, expr: {:integer, %{value: 2}}}} = s2
    assert {:return, %{expr: {:identifier, %{name: "value"}}}} = s3
  end

  test "parses void return" do
    source = """
    fn main() -> void {
        return;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{return_type: :void, body: {:block, %{stmts: [stmt]}}}} = func
    assert {:return, %{expr: nil}} = stmt
  end

  test "parses type definition and struct literal" do
    source = """
    type User {
        id: i32,
        name: String
    }

    fn main() -> i32 {
        let user: User = { id = 1, name = "Peter" };
        return 1;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{types: [type_def], functions: [func]}} = ast
    assert {:type_def, %{name: "User", fields: fields}} = type_def
    assert length(fields) == 2
    assert {:function, %{name: "main"}} = func
  end

  test "parses field access" do
    source = """
    type User {
        id: i32
    }

    fn main() -> i32 {
        let user: User = { id = 1 };
        return user->id;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [_s1, s2]}}}} = func
    assert {:return, %{expr: {:field, %{name: "id"}}}} = s2
  end

  test "parses field assignment" do
    source = """
    type User {
        id: i32
    }

    fn main() -> i32 {
        let mut user: User = { id = 1 };
        user->id = 2;
        return user->id;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [_s1, s2, _s3]}}}} = func
    assert {:assign, %{target: {:field, %{name: "id"}}}} = s2
  end

  test "parses match expression" do
    source = """
    fn main() -> i32 {
        return match (1) {
            case 1 if 1 < 2 => 2,
            case _ => 0
        };
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [stmt]}}}} = func
    assert {:return, %{expr: {:match, %{cases: cases}}}} = stmt
    assert length(cases) == 2
    assert %{pattern: {:wildcard, _}} = List.last(cases)
    assert %{guard: {:binary, %{op: :lt}}} = hd(cases)
  end

  test "parses if statement" do
    source = """
    fn main() -> i32 {
        if (true) {
            return 1;
        } else {
            return 2;
        }
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [stmt]}}}} = func
    assert {:if_stmt, %{cond: {:bool, _}}} = stmt
  end

  test "parses if expression" do
    source = """
    fn main() -> i32 {
        let value = if (true) { 1; } else { 2; };
        return value;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [s1, _s2]}}}} = func
    assert {:let, %{expr: {:if_expr, _}}} = s1
  end
end
