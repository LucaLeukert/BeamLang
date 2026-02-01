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

  test "rejects empty interpolation" do
    source = """
    fn main() -> number {
        println("${}");
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    assert {:error, error} = Parser.parse(tokens)
    assert error.kind == :parser
  end

  test "parses generic function definition" do
    source = """
    fn test<T>(opt: Optional<T>) -> T {
        return opt->unwrap(0);
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{name: "test", type_params: ["T"], params: [param], return_type: {:named, "T"}}} = func
    assert %{name: "opt", type: {:generic, {:named, "Optional"}, [{:named, "T"}]}} = param
  end

  test "parses type definition named String" do
    source = """
    type String {
        data: any
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{types: [type_def]}} = ast
    assert {:type_def, %{name: "String"}} = type_def
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

  test "parses lambda with self parameter" do
    source = """
    fn main() -> number {
        let identity = fn(self: number) -> number { return self; };
        return identity(1);
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [s1, _s2]}}}} = func
    assert {:let, %{expr: {:lambda, %{params: [param], return_type: :number}}}} = s1
    assert %{name: "self"} = param
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

  test "parses for loop over iterator" do
    source = """
    fn main() -> number {
        for (item in "hi"->chars()) { break; }
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [stmt | _]}}}} = func
    assert {:for, %{collection: {:method_call, %{name: "chars"}}}} = stmt
  end

  test "parses println with number" do
    source = """
    fn main() -> number {
        println(1);
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [stmt | _]}}}} = func
    assert {:expr, %{expr: {:call, %{name: "println", args: [arg]}}}} = stmt
    assert {:integer, %{value: 1}} = arg
  end

  test "parses internal function" do
    source = """
    internal fn helper(value: number) -> number {
        return value;
    }

    fn main() -> number {
        return helper(1);
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [helper, _main]}} = ast
    assert {:function, %{name: "helper", internal: true}} = helper
  end

  test "parses internal fields in type definition" do
    source = """
    type Container<T> {
        internal data: any,
        get: fn(Container<T>) -> T
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{types: [type_def]}} = ast
    assert {:type_def, %{name: "Container", fields: [data_field, get_field]}} = type_def
    assert %{name: "data", type: :any, internal: true} = data_field
    assert %{name: "get", type: {:fn, _, _}, internal: false} = get_field
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

  test "parses call with type arguments" do
    source = """
    fn main(args: [String]) -> number {
        let parsed = parse_args<Args>(args);
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [stmt, _]}}}} = func
    assert {:let, %{expr: {:call, %{name: "parse_args", type_args: [type_arg], args: [_]}}}} = stmt
    assert {:named, "Args"} = type_arg
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

  test "parses list method calls" do
    source = """
    fn main() -> number {
        let nums = list_new();
        let nums2 = nums->push(1);
        let len = nums2->length();
        return len;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{name: "main", body: {:block, %{stmts: stmts}}}} = func
    assert length(stmts) == 4

    # let nums = list_new();
    assert {:let, %{name: "nums", expr: {:call, %{name: "list_new", args: []}}}} = Enum.at(stmts, 0)

    # let nums2 = nums->push(1);
    assert {:let, %{name: "nums2", expr: {:method_call, %{target: {:identifier, %{name: "nums"}}, name: "push", args: [_]}}}} = Enum.at(stmts, 1)

    # let len = nums2->length();
    assert {:let, %{name: "len", expr: {:method_call, %{target: {:identifier, %{name: "nums2"}}, name: "length", args: []}}}} = Enum.at(stmts, 2)

    # return len;
    assert {:return, %{expr: {:identifier, %{name: "len"}}}} = Enum.at(stmts, 3)
  end

  test "parses list literal syntax" do
    source = """
    fn main() -> number {
        let nums: [number] = [1, 2, 3];
        let empty: [number] = [];
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{name: "main", body: {:block, %{stmts: stmts}}}} = func
    assert length(stmts) == 3

    # let nums: [number] = [1, 2, 3];
    assert {:let, %{name: "nums", type: {:generic, {:named, "List"}, [:number]}, expr: {:list_literal, %{elements: elems}}}} = Enum.at(stmts, 0)
    assert length(elems) == 3

    # let empty: [number] = [];
    assert {:let, %{name: "empty", type: {:generic, {:named, "List"}, [:number]}, expr: {:list_literal, %{elements: []}}}} = Enum.at(stmts, 1)
  end

  test "parses nested list type" do
    source = """
    fn main() -> [[number]] {
        return [];
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{name: "main", return_type: {:generic, {:named, "List"}, [{:generic, {:named, "List"}, [:number]}]}}} = func
  end

  test "parses main with args parameter" do
    source = """
    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{name: "main", params: [param], return_type: :number}} = func
    assert %{name: "args", type: {:generic, {:named, "List"}, [:String]}} = param
  end

  test "parses error definition" do
    source = """
    error FileError {
        path: String,
        message: String
    }

    fn main() -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{errors: [error], functions: [_]}} = ast
    assert {:error_def, %{name: "FileError", fields: fields, exported: false}} = error
    assert [%{name: "path", type: :String}, %{name: "message", type: :String}] = fields
  end

  test "parses exported error definition" do
    source = """
    export error ParseError {
        line: number,
        column: number,
        message: String
    }

    fn main() -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{errors: [error], functions: [_]}} = ast
    assert {:error_def, %{name: "ParseError", exported: true, fields: fields}} = error
    assert length(fields) == 3
    assert %{name: "line", type: :number} = Enum.at(fields, 0)
    assert %{name: "column", type: :number} = Enum.at(fields, 1)
    assert %{name: "message", type: :String} = Enum.at(fields, 2)
  end

  test "parses range expression" do
    source = """
    fn main() -> number {
        for (i in 1..10) {
            println(i);
        }
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [for_stmt | _]}}}} = func
    assert {:for, %{name: "i", collection: collection}} = for_stmt
    assert {:range, %{start: {:integer, %{value: 1}}, end: {:integer, %{value: 10}}}} = collection
  end

  test "parses for loop over list literal" do
    source = """
    fn main() -> number {
        for (x in [1, 2, 3]) {
            println(x);
        }
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [for_stmt | _]}}}} = func
    assert {:for, %{name: "x", collection: {:list_literal, %{elements: elements}}}} = for_stmt
    assert length(elements) == 3
  end

  test "parses mutable parameter" do
    source = """
    fn update(mut value: number) -> number {
        value = value + 1;
        return value;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{name: "update", params: [param]}} = func
    assert %{name: "value", type: :number, mutable: true} = param
  end

  test "parses mixed mutable and immutable parameters" do
    source = """
    fn process(x: number, mut y: number, z: String) -> number {
        y = y + x;
        return y;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{params: [p1, p2, p3]}} = func
    assert %{name: "x", mutable: false} = p1
    assert %{name: "y", mutable: true} = p2
    assert %{name: "z", mutable: false} = p3
  end

  test "parses type with operator overloading" do
    source = """
    type Path {
        path: String,
        operator /: fn(Path, String) -> Path,
        op_div: fn(Path, String) -> Path
    }

    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{types: [type_def], functions: _}} = ast
    assert {:type_def, %{name: "Path", fields: fields, operators: operators}} = type_def
    assert length(fields) == 2
    assert length(operators) == 1
    [op] = operators
    assert op.op == :div
    assert {:fn, [{:named, "Path"}, :String], {:named, "Path"}} = op.type
  end

  test "parses type with multiple operator overloads" do
    source = """
    type Vec2 {
        x: number,
        y: number,
        operator +: fn(Vec2, Vec2) -> Vec2,
        operator -: fn(Vec2, Vec2) -> Vec2,
        operator *: fn(Vec2, number) -> Vec2,
        op_add: fn(Vec2, Vec2) -> Vec2,
        op_sub: fn(Vec2, Vec2) -> Vec2,
        op_mul: fn(Vec2, number) -> Vec2
    }

    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{types: [type_def], functions: _}} = ast
    assert {:type_def, %{name: "Vec2", fields: fields, operators: operators}} = type_def
    assert length(fields) == 5  # x, y, op_add, op_sub, op_mul
    assert length(operators) == 3
    ops_by_name = Enum.map(operators, & &1.op)
    assert :add in ops_by_name
    assert :sub in ops_by_name
    assert :mul in ops_by_name
  end
end
