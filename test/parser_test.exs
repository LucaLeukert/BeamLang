defmodule BeamLang.ParserTest do
  use ExUnit.Case, async: true

  alias BeamLang.{Lexer, Parser}

  test "parses minimal function" do
    source = """
    fn main() -> number {
        return 42;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{name: "main", return_type: :number, body: {:block, %{stmts: [stmt]}}}} = func
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
    fn main() -> number {
        return helper(1);
    }

    fn helper(value: number) -> number {
        return 1;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [main, helper]}} = ast
    assert {:function, %{name: "main", return_type: :number, body: {:block, %{stmts: [stmt1]}}}} = main
    assert {:return, %{expr: {:call, %{name: "helper", args: [arg]}}}} = stmt1
    assert {:integer, %{value: 1}} = arg
    assert {:function, %{name: "helper", return_type: :number, params: [_], body: {:block, %{stmts: [stmt2]}}}} = helper
    assert {:return, %{expr: {:integer, %{value: 1}}}} = stmt2
  end

  test "parses call statement before return" do
    source = """
    fn main() -> number {
        helper(1);
        return 1;
    }

    fn helper(value: number) -> number {
        return 2;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [main, helper]}} = ast
    assert {:function, %{name: "main", return_type: :number, body: {:block, %{stmts: [s1, s2]}}}} = main
    assert {:expr, %{expr: {:call, %{name: "helper", args: [arg]}}}} = s1
    assert {:integer, %{value: 1}} = arg
    assert {:return, %{expr: {:integer, %{value: 1}}}} = s2
    assert {:function, %{name: "helper", return_type: :number, params: [_], body: {:block, %{stmts: [h1]}}}} = helper
    assert {:return, %{expr: {:integer, %{value: 2}}}} = h1
  end

  test "parses let and identifier return" do
    source = """
    fn main() -> number {
        let value = 3;
        return value;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{name: "main", return_type: :number, body: {:block, %{stmts: [s1, s2]}}}} = func
    assert {:let, %{name: "value", mutable: false, expr: {:integer, %{value: 3}}}} = s1
    assert {:return, %{expr: {:identifier, %{name: "value"}}}} = s2
  end

  test "parses assignment statement" do
    source = """
    fn main() -> number {
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

  test "parses import braces and alias" do
    source = """
    import math.{Pair, add};
    import math as m;

    fn main() -> number {
        let pair: Pair = { left = 1, right = 2 };
        return m::add(pair->left, pair->right);
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{imports: [imp1, imp2]}} = ast
    assert {:import, %{module: "math", alias: nil, items: items}} = imp1
    assert Enum.map(items, & &1.name) == ["Pair", "add"]
    assert {:import, %{module: "math", alias: "m", items: :none}} = imp2
  end

  test "parses function type in struct fields" do
    source = """
    type Iterator<T> {
        next: fn(Iterator<T>) -> Optional<T>,
        fold: fn(Iterator<T>, any, fn(any, T) -> any) -> any
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{types: [type_def]}} = ast
    assert {:type_def, %{name: "Iterator", fields: fields}} = type_def
    assert %{name: "next", type: {:fn, [_], _}} = Enum.at(fields, 0)
    assert %{name: "fold", type: {:fn, [_, _, {:fn, [_, _], _}], _}} = Enum.at(fields, 1)
  end

  test "parses lambda expression" do
    source = """
    fn main() -> number {
        let add_one = fn(x: number) -> number { return x + 1; };
        return add_one(1);
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [s1, _s2]}}}} = func
    assert {:let, %{expr: {:lambda, %{params: [_], return_type: :number}}}} = s1
  end

  test "parses method call" do
    source = """
    fn main() -> number {
        let name = find_name(1);
        let un = name->unwrap("Unknown");
        return 1;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [_s1, s2, _s3]}}}} = func
    assert {:let, %{expr: {:method_call, %{name: "unwrap"}}}} = s2
  end

  test "parses type definition and struct literal" do
    source = """
    type User {
        id: number,
        name: String
    }

    fn main() -> number {
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
        id: number
    }

    fn main() -> number {
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
        id: number
    }

    fn main() -> number {
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
    fn main() -> number {
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
    fn main() -> number {
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
    fn main() -> number {
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
