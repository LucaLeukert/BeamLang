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
    type Stream<T> {
        next: fn(Stream<T>) -> Optional<T>,
        fold: fn(Stream<T>, any, fn(any, T) -> any) -> any
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{types: [type_def]}} = ast
    assert {:type_def, %{name: "Stream", fields: fields}} = type_def
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

  test "parses for loop over list" do
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

  test "parses match arrows for blocks and expressions" do
    source = """
    fn main() -> number {
        let value = match (true) {
            case true -> { 1; },
            case false => 2
        };
        return value;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [stmt, _]}}}} = func
    assert {:let, %{expr: {:match, %{cases: [case1, case2]}}}} = stmt
    assert {:block_expr, _} = case1.body
    assert {:integer, _} = case2.body
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

  test "parses if statement with assignments" do
    source = """
    fn main() -> number {
        let mut value = 1;
        if (true) {
            value = 2;
        } else {
            value = 3;
        }
        return value;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [s1, s2, _s3]}}}} = func
    assert {:let, %{name: "value", mutable: true}} = s1
    assert {:if_stmt, %{then_block: {:block, %{stmts: [t1]}}, else_branch: {:else_block, %{block: {:block, %{stmts: [e1]}}}}}} = s2
    assert {:assign, %{target: {:identifier, %{name: "value"}}}} = t1
    assert {:assign, %{target: {:identifier, %{name: "value"}}}} = e1
  end

  test "parses multiline string literal" do
    source =
      "fn main() -> number {\n" <>
        "    let usage = \"\"\"Usage: hello\n" <>
        "Line two\"\"\";\n" <>
        "    return 0;\n" <>
        "}\n"

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [func]}} = ast
    assert {:function, %{body: {:block, %{stmts: [s1, _s2]}}}} = func
    assert {:let, %{name: "usage", expr: {:string, %{value: value}}}} = s1
    assert value == "Usage: hello\nLine two"
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

  test "parses type with operator declaration" do
    source = """
    type Path {
        path: String,
        operator /: fn(Path, String) -> Path
    }

    fn path_join(self: Path, segment: String) -> Path {
        return { path = "test", operator / = path_join };
    }

    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{types: [type_def], functions: _}} = ast
    assert {:type_def, %{name: "Path", fields: fields, operators: operators}} = type_def
    assert length(fields) == 1  # just path field
    assert length(operators) == 1
    [op_decl] = operators
    assert op_decl.op == :div
    assert op_decl.type == {:fn, [{:named, "Path"}, :String], {:named, "Path"}}
  end

  test "parses struct literal with operator binding" do
    source = """
    type Vec2 {
        x: number,
        y: number,
        operator +: fn(Vec2, Vec2) -> Vec2
    }

    fn vec2_add(a: Vec2, b: Vec2) -> Vec2 { return { x = 0, y = 0, operator + = vec2_add }; }

    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{types: [type_def], functions: functions}} = ast
    assert {:type_def, %{name: "Vec2", fields: fields, operators: operators}} = type_def
    assert length(fields) == 2  # x, y
    assert length(operators) == 1
    [op_decl] = operators
    assert op_decl.op == :add

    # Check that the struct literal has the operator binding
    [{:function, %{body: {:block, %{stmts: [{:return, %{expr: {:struct, struct_info}}}]}}}}, _] = functions
    assert length(struct_info.operators) == 1
    [op] = struct_info.operators
    assert op.op == :add
    assert op.func == "vec2_add"
  end

  test "parses simple enum definition" do
    source = """
    enum Color {
        Red,
        Green,
        Blue
    }

    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{enums: [enum_def]}} = ast
    assert {:enum_def, %{name: "Color", variants: variants}} = enum_def
    assert length(variants) == 3
    [red, green, blue] = variants
    assert {:enum_variant, %{name: "Red", fields: []}} = red
    assert {:enum_variant, %{name: "Green", fields: []}} = green
    assert {:enum_variant, %{name: "Blue", fields: []}} = blue
  end

  test "parses enum with fields" do
    source = """
    enum Shape {
        Circle { radius: number },
        Rectangle { width: number, height: number },
        Point
    }

    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{enums: [enum_def]}} = ast
    assert {:enum_def, %{name: "Shape", variants: variants}} = enum_def
    assert length(variants) == 3
    [circle, rectangle, point] = variants
    assert {:enum_variant, %{name: "Circle", fields: [%{name: "radius", type: :number}]}} = circle
    assert {:enum_variant, %{name: "Rectangle", fields: [%{name: "width"}, %{name: "height"}]}} = rectangle
    assert {:enum_variant, %{name: "Point", fields: []}} = point
  end

  test "parses enum variant expressions" do
    source = """
    enum Color { Red, Green }

    fn main(args: [String]) -> number {
        let c = Color::Red;
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [{:function, func}]}} = ast
    assert {:block, %{stmts: [{:let, %{expr: expr}}, _]}} = func.body
    assert {:enum_variant, %{enum_name: "Color", variant: "Red", fields: []}} = expr
  end

  test "parses enum variant with fields expression" do
    source = """
    enum Shape { Circle { radius: number } }

    fn main(args: [String]) -> number {
        let s = Shape::Circle { radius = 5.0 };
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [{:function, func}]}} = ast
    assert {:block, %{stmts: [{:let, %{expr: expr}}, _]}} = func.body
    assert {:enum_variant, %{enum_name: "Shape", variant: "Circle", fields: fields}} = expr
    assert [{"radius", {:float, %{value: 5.0}}}] = fields
  end

  test "parses enum pattern in match" do
    source = """
    enum Color { Red, Green }

    fn main(args: [String]) -> number {
        let c = Color::Red;
        return match (c) {
            case Color::Red => 1,
            case Color::Green => 2,
            case _ => 0
        };
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [{:function, func}]}} = ast
    assert {:block, %{stmts: [_, {:return, %{expr: {:match, match_expr}}}]}} = func.body
    assert [case1, case2, _wildcard] = match_expr.cases
    assert %{pattern: {:enum_pattern, %{enum_name: "Color", variant: "Red"}}} = case1
    assert %{pattern: {:enum_pattern, %{enum_name: "Color", variant: "Green"}}} = case2
  end

  # Pattern matching in function parameters tests

  test "parses struct pattern in function parameter" do
    source = """
    type Point { x: number, y: number }

    fn get_x(Point { x, y }: Point) -> number {
        return x;
    }

    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: functions}} = ast
    get_x = Enum.find(functions, fn {:function, %{name: n}} -> n == "get_x" end)
    assert {:function, %{params: [param]}} = get_x
    assert %{pattern: {:struct_pattern, %{name: "Point", fields: fields}}, type: {:named, "Point"}} = param
    assert [%{name: "x"}, %{name: "y"}] = fields
  end

  test "parses tuple pattern in function parameter" do
    source = """
    fn first((a, b): (number, String)) -> number {
        return a;
    }

    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: functions}} = ast
    first = Enum.find(functions, fn {:function, %{name: n}} -> n == "first" end)
    assert {:function, %{params: [param]}} = first
    assert %{pattern: {:tuple_pattern, %{elements: elements}}, type: {:tuple, [:number, :String]}} = param
    assert [pat_identifier: %{name: "a"}, pat_identifier: %{name: "b"}] = elements
  end

  test "parses mixed regular and pattern parameters" do
    source = """
    type Point { x: number, y: number }

    fn add_offset(Point { x, y }: Point, offset: number) -> number {
        return x + offset;
    }

    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: functions}} = ast
    add_offset = Enum.find(functions, fn {:function, %{name: n}} -> n == "add_offset" end)
    assert {:function, %{params: [pattern_param, regular_param]}} = add_offset
    assert %{pattern: {:struct_pattern, _}} = pattern_param
    assert %{name: "offset", type: :number} = regular_param
  end

  test "parses type with field annotations" do
    source = """
    type Args {
        @required()
        @description("Input file")
        file: String,

        @short("n")
        @long("count")
        @default(10)
        count: number,

        @flag
        @short("v")
        verbose: bool
    }

    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{types: [type_def]}} = ast
    assert {:type_def, %{name: "Args", fields: fields}} = type_def

    # Check file field annotations
    file_field = Enum.find(fields, fn f -> f.name == "file" end)
    assert file_field != nil
    assert length(file_field.annotations) == 2
    assert Enum.any?(file_field.annotations, fn a -> a.name == "required" end)
    assert Enum.any?(file_field.annotations, fn a -> a.name == "description" and a.args == ["Input file"] end)

    # Check count field annotations
    count_field = Enum.find(fields, fn f -> f.name == "count" end)
    assert count_field != nil
    assert length(count_field.annotations) == 3
    assert Enum.any?(count_field.annotations, fn a -> a.name == "short" and a.args == ["n"] end)
    assert Enum.any?(count_field.annotations, fn a -> a.name == "long" and a.args == ["count"] end)
    assert Enum.any?(count_field.annotations, fn a -> a.name == "default" and a.args == [10] end)

    # Check verbose field annotations
    verbose_field = Enum.find(fields, fn f -> f.name == "verbose" end)
    assert verbose_field != nil
    assert length(verbose_field.annotations) == 2
    assert Enum.any?(verbose_field.annotations, fn a -> a.name == "flag" end)
    assert Enum.any?(verbose_field.annotations, fn a -> a.name == "short" and a.args == ["v"] end)
  end

  test "parses default string annotation" do
    source = """
    type Args {
        @default("-")
        file: String,

        @flag
        @short("l")
        lines: bool
    }

    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{types: [type_def]}} = ast
    assert {:type_def, %{name: "Args", fields: fields}} = type_def

    file_field = Enum.find(fields, fn f -> f.name == "file" end)
    assert file_field != nil
    assert Enum.any?(file_field.annotations, fn a -> a.name == "default" and a.args == ["-"] end)

    lines_field = Enum.find(fields, fn f -> f.name == "lines" end)
    assert lines_field != nil
    assert Enum.any?(lines_field.annotations, fn a -> a.name == "flag" end)
    assert Enum.any?(lines_field.annotations, fn a -> a.name == "short" and a.args == ["l"] end)
  end

  test "parses external function declaration" do
    source = """
    @external(elixir, "BeamLang.Runtime", "http_request")
    fn http_request_data(method: String, url: String) -> String!String;

    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{functions: [ext_func, _main]}} = ast
    assert {:function, %{name: "http_request_data", body: nil, external: external}} = ext_func
    assert external.language == "elixir"
    assert external.module == "BeamLang.Runtime"
    assert external.function == "http_request"
  end

  test "parses type without field annotations (backwards compatible)" do
    source = """
    type Simple {
        name: String,
        value: number
    }

    fn main(args: [String]) -> number {
        return 0;
    }
    """

    {:ok, tokens} = Lexer.tokenize(source)
    {:ok, ast} = Parser.parse(tokens)

    assert {:program, %{types: [type_def]}} = ast
    assert {:type_def, %{name: "Simple", fields: fields}} = type_def

    name_field = Enum.find(fields, fn f -> f.name == "name" end)
    assert name_field.annotations == []

    value_field = Enum.find(fields, fn f -> f.name == "value" end)
    assert value_field.annotations == []
  end
end
