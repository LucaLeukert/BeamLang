defmodule BeamLang.IntegrationTest do
  use ExUnit.Case, async: true

  alias BeamLang

  test "compiles and runs main" do
    source = """
    fn main(args: [String]) -> number {
        return 42;
    }
    """

    assert {:ok, 42} == BeamLang.run_source(source)
  end

  test "supports multiple functions" do
    source = """
    fn helper(value: number) -> number {
        return 1;
    }

    fn main(args: [String]) -> number {
        return helper(42);
    }
    """

    assert {:ok, 1} == BeamLang.run_source(source)
  end

  test "runs function call in return" do
    source = """
    fn helper(value: number) -> number {
        return 7;
    }

    fn main(args: [String]) -> number {
        return helper(1);
    }
    """

    assert {:ok, 7} == BeamLang.run_source(source)
  end

  test "runs stdlib println" do
    source = """
    fn main(args: [String]) -> number {
        println("hello");
        return 0;
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source)
  end

  test "runs let-bound value" do
    source = """
    fn main(args: [String]) -> number {
        let value = 5;
        return value;
    }
    """

    assert {:ok, 5} == BeamLang.run_source(source)
  end

  test "supports mutable reassignment" do
    source = """
    fn main(args: [String]) -> number {
        let mut value = 1;
        value = 3;
        return value;
    }
    """

    assert {:ok, 3} == BeamLang.run_source(source)
  end

  test "propagates mutable assignments across if blocks" do
    source = """
    fn main(args: [String]) -> number {
        let mut x = 1;
        let mut y = 10;

        if (true) {
            x = 2;
            y = 20;
        } else {
            x = 3;
        }

        return x + y;
    }
    """

    assert {:ok, 22} == BeamLang.run_source(source)
  end

  test "propagates mutable assignments with return in if branch" do
    source = """
    fn main(args: [String]) -> number {
        let mut x = 1;
        if (true) {
            x = 2;
            return x;
        } else {
            x = 3;
        }
        return x;
    }
    """

    assert {:ok, 2} == BeamLang.run_source(source)
  end

  test "propagates mutable assignments with break in if branch" do
    source = """
    fn main(args: [String]) -> number {
        let mut x = 0;
        loop {
            if (x == 0) {
                x = 1;
                break;
            } else {
                x = 2;
            }
        }
        return x;
    }
    """

    assert {:ok, 1} == BeamLang.run_source(source)
  end

  test "supports void return" do
    source = """
    fn main(args: [String]) -> void {
        return;
    }
    """

    assert {:error, [error]} = BeamLang.run_source(source)
    assert error.message == "main must return number, got void."
  end

  test "main return type error keeps source file span" do
    source = """
    fn main(args: [String]) -> void {
        return;
    }
    """

    filename = "/tmp/main_return_span.beam"
    assert {:error, [error]} = BeamLang.compile_source(source, filename)
    assert error.message == "main must return number, got void."
    assert error.span.file_id == filename
    assert error.span.end >= error.span.start
  end

  test "supports struct literal with type annotation" do
    source = """
    type User {
        id: number,
        name: String
    }

    fn main(args: [String]) -> number {
        let user: User = { id = 1, name = "Peter" };
        return 1;
    }
    """

    assert {:ok, 1} == BeamLang.run_source(source)
  end

  test "supports struct field access" do
    source = """
    type User {
        id: number,
        name: String
    }

    fn main(args: [String]) -> number {
        let user: User = { id = 2, name = "Peter" };
        return user->id;
    }
    """

    assert {:ok, 2} == BeamLang.run_source(source)
  end

  test "supports struct field assignment" do
    source = """
    type User {
        id: number,
        name: String
    }

    fn main(args: [String]) -> number {
        let mut user: User = { id = 1, name = "Peter" };
        user->id = 3;
        return user->id;
    }
    """

    assert {:ok, 3} == BeamLang.run_source(source)
  end

  test "supports chained method calls" do
    source = """
    fn main(args: [String]) -> number {
        let value = [1, 2]->push(3)->first()->unwrap(0);
        return value;
    }
    """

    assert {:ok, 1} == BeamLang.run_source(source)
  end

  test "supports lambda return using local let binding" do
    source = """
    fn main(args: [String]) -> number {
        let get_value = fn() -> number {
            let value = 7;
            return value;
        };
        return get_value();
    }
    """

    assert {:ok, 7} == BeamLang.run_source(source)
  end

  test "rejects non-i32 main" do
    source = """
    fn main(args: [String]) -> bool {
        return true;
    }
    """

    assert {:error, [error]} = BeamLang.run_source(source)
    assert error.message == "main must return number, got bool."
  end

  test "runs match expression" do
    source = """
    fn main(args: [String]) -> number {
        return match (1) {
            case 1 if 1 == 1 => 4,
            case _ => 0
        };
    }
    """

    assert {:ok, 4} == BeamLang.run_source(source)
  end

  test "rejects non-exhaustive match expression" do
    source = """
    fn main(args: [String]) -> number {
        return match (true) {
            case true => 1
        };
    }
    """

    assert {:error, [error]} = BeamLang.run_source(source)
    assert error.message == "Non-exhaustive match. Add a 'case _' or cover all variants."
  end

  test "rejects non-exhaustive string match expression" do
    source = """
    fn main(args: [String]) -> number {
        return match ("ok") {
            case "ok" => 1
        };
    }
    """

    assert {:error, [error]} = BeamLang.run_source(source)
    assert error.message == "Non-exhaustive match. Add a 'case _' or cover all variants."
  end

  test "parse_args returns err on argument mismatch" do
    source = """
    import args.*;

    type Args {
        path: String
    }

    fn main(args: [String]) -> number {
        let parsed = parse_args<Args>(args);

        return match (parsed) {
            case!ok _ => 1,
            case!err _ => 0
        };
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source)
  end

  test "runs if expression" do
    source = """
    fn main(args: [String]) -> number {
        let value = if (true) { 1; } else { 2; };
        return value;
    }
    """

    assert {:ok, 1} == BeamLang.run_source(source)
  end

  test "runs loop with break" do
    source = """
    fn main(args: [String]) -> number {
        loop { break; }
        return 0;
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source)
  end

  test "runs while with false condition" do
    source = """
    fn main(args: [String]) -> number {
        while (false) { break; }
        return 0;
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source)
  end

  test "runs for over list" do
    source = """
    fn main(args: [String]) -> number {
        for (item in "hi"->chars()) { break; }
        return 0;
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source)
  end

  test "typeof returns String struct" do
    source = """
    fn main(args: [String]) -> number {
        let t = typeof(10);
        return t->length();
    }
    """

    assert {:ok, 6} == BeamLang.run_source(source)
  end

  test "runs while loop with mutable variable increment" do
    source = """
    fn main(args: [String]) -> number {
        let mut i = 0;
        while (i < 3) {
            i = i + 1;
        }
        return i;
    }
    """

    assert {:ok, 3} == BeamLang.run_source(source)
  end

  test "runs while loop with multiple statements" do
    source = """
    fn main(args: [String]) -> number {
        let mut i = 0;
        let mut sum = 0;
        while (i < 5) {
            sum = sum + i;
            i = i + 1;
        }
        return sum;
    }
    """

    # sum = 0 + 1 + 2 + 3 + 4 = 10
    assert {:ok, 10} == BeamLang.run_source(source)
  end

  test "runs while loop with println" do
    source = """
    fn main(args: [String]) -> number {
        let mut i = 0;
        while (i < 3) {
            println(i);
            i = i + 1;
        }
        return i;
    }
    """

    assert {:ok, 3} == BeamLang.run_source(source)
  end

  test "parse_args with annotations and flags" do
    source = """
    import args.*;

    type Args {
        @required()
        @description("Input file")
        file: String,

        @flag
        @short("v")
        @long("verbose")
        verbose: bool
    }

    fn check(opts: Args) -> number {
        if (opts->verbose) {
            return 1;
        }
        return 0;
    }

    fn main(args: [String]) -> number {
        let parsed = parse_args<Args>(args);

        return match (parsed) {
            case!ok opts => check(opts),
            case!err _ => 99
        };
    }
    """

    # With -v flag
    assert {:ok, 1} == BeamLang.run_source(source, ~w[test.txt -v])
    # Without flag
    assert {:ok, 0} == BeamLang.run_source(source, ~w[test.txt])
    # Missing required arg
    assert {:ok, 99} == BeamLang.run_source(source, ~w[])
  end

  test "parse_args detects --help" do
    source = """
    import args.*;

    type Args {
        @required()
        file: String
    }

    fn check_err(err: ArgsError) -> number {
        if (err->message == "--help") {
            return 42;
        }
        return 1;
    }

    fn main(args: [String]) -> number {
        let parsed = parse_args<Args>(args);

        return match (parsed) {
            case!ok _ => 0,
            case!err err => check_err(err)
        };
    }
    """

    assert {:ok, 42} == BeamLang.run_source(source, ~w[--help])
    assert {:ok, 42} == BeamLang.run_source(source, ~w[-h])
  end

  test "parse_args supports combined short flags" do
    source = """
    import args.*;

    type Args {
        @flag
        @short("a")
        alpha: bool,

        @flag
        @short("b")
        beta: bool,

        @flag
        @short("c")
        gamma: bool
    }

    fn count_flags(opts: Args) -> number {
        let mut count = 0;
        if (opts->alpha) { count = count + 1; }
        if (opts->beta) { count = count + 1; }
        if (opts->gamma) { count = count + 1; }
        return count;
    }

    fn main(args: [String]) -> number {
        let parsed = parse_args<Args>(args);

        return match (parsed) {
            case!ok opts => count_flags(opts),
            case!err _ => 99
        };
    }
    """

    # Combined -abc
    assert {:ok, 3} == BeamLang.run_source(source, ~w[-abc])
    # Separate flags
    assert {:ok, 2} == BeamLang.run_source(source, ~w[-a -c])
    # No flags
    assert {:ok, 0} == BeamLang.run_source(source, ~w[])
  end

  test "parse_args supports -- separator" do
    source = """
    import args.*;

    type Args {
        @required()
        file: String,

        @flag
        @short("v")
        @long("verbose")
        verbose: bool
    }

    fn check(opts: Args) -> number {
        if (opts->verbose) {
            return 1;
        }
        return 0;
    }

    fn main(args: [String]) -> number {
        let parsed = parse_args<Args>(args);

        return match (parsed) {
            case!ok opts => check(opts),
            case!err _ => 99
        };
    }
    """

    # -v after -- is treated as positional, not a flag
    assert {:ok, 0} == BeamLang.run_source(source, ~w[-- -v])
  end

  test "usage generates help text" do
    source = """
    import args.*;

    type Args {
        @required()
        @description("Input file")
        file: String,

        @flag
        @short("v")
        @long("verbose")
        @description("Enable verbose output")
        verbose: bool
    }

    fn main(args: [String]) -> number {
        let help = usage<Args>("myapp");
        if (help->contains("myapp")) {
            if (help->contains("verbose")) {
                return 0;
            }
        }
        return 1;
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source)
  end

  test "pure-BeamLang args helpers: classify and find" do
    source = """
    import args.*;

    fn main(args: [String]) -> number {
        // is_help should detect --help
        let h1 = is_help("--help");
        let h2 = is_help("-h");
        let h3 = is_help("--verbose");

        guard (h1) else { return 1; }
        guard (h2) else { return 2; }
        guard (h3 == false) else { return 3; }

        // has_help_flag on list
        let has = has_help_flag(args);
        guard (has) else { return 4; }

        // is_long_opt / is_short_opt / is_separator
        guard (is_long_opt("--verbose")) else { return 5; }
        guard (is_long_opt("-v") == false) else { return 6; }
        guard (is_short_opt("-v")) else { return 7; }
        guard (is_short_opt("--verbose") == false) else { return 8; }
        guard (is_separator("--")) else { return 9; }

        return 0;
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source, ~w[--help])
  end

  test "pure-BeamLang args helpers: find_flag and find_option" do
    source = """
    import args.*;

    fn main(args: [String]) -> number {
        let verbose = find_flag(args, "verbose", "v");
        guard (verbose) else { return 1; }

        let count_opt = find_option(args, "count", "n");
        let count_val = count_opt->unwrap("0");
        guard (count_val == "5") else { return 2; }

        return 0;
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source, ~w[--verbose --count 5])
  end

  test "pure-BeamLang args helpers: parse_long_opt and parse_short_opt" do
    source = """
    import args.*;

    fn check_long() -> number {
        let parsed = parse_long_opt("--name=value");
        return match (parsed) {
            case?some flag -> {
                guard (flag->name == "name") else { return 1; }
                guard (flag->has_value) else { return 2; }
                guard (flag->value == "value") else { return 3; }
                0;
            },
            case?none => 10
        };
    }

    fn check_short() -> number {
        let parsed = parse_short_opt("-n5");
        return match (parsed) {
            case?some flag -> {
                guard (flag->name == "n") else { return 4; }
                guard (flag->has_value) else { return 5; }
                guard (flag->value == "5") else { return 6; }
                0;
            },
            case?none => 11
        };
    }

    fn main(args: [String]) -> number {
        let r1 = check_long();
        guard (r1 == 0) else { return r1; }

        let r2 = check_short();
        guard (r2 == 0) else { return r2; }

        return 0;
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source)
  end

  test "pure-BeamLang args helpers: split_at_separator" do
    source = """
    import args.*;

    fn main(args: [String]) -> number {
        let split = split_at_separator(args);
        let before_len = split->before->length();
        let after_len = split->after->length();

        guard (before_len == 2) else { return 1; }
        guard (after_len == 2) else { return 2; }

        return 0;
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source, ~w[hello world -- foo bar])
  end
end
