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

  test "supports void return" do
    source = """
    fn main(args: [String]) -> void {
        return;
    }
    """

    assert {:error, [error]} = BeamLang.run_source(source)
    assert error.message == "main must return number, got void."
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

  test "runs for over iterator" do
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
end
