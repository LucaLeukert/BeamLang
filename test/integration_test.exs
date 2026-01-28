defmodule BeamLang.IntegrationTest do
  use ExUnit.Case, async: true

  alias BeamLang

  test "compiles and runs main" do
    source = """
    fn main() -> i32 {
        return 42;
    }
    """

    assert {:ok, 42} == BeamLang.run_source(source)
  end

  test "supports multiple functions" do
    source = """
    fn helper(value: i32) -> i32 {
        return 1;
    }

    fn main() -> i32 {
        return helper(42);
    }
    """

    assert {:ok, 1} == BeamLang.run_source(source)
  end

  test "runs function call in return" do
    source = """
    fn helper(value: i32) -> i32 {
        return 7;
    }

    fn main() -> i32 {
        return helper(1);
    }
    """

    assert {:ok, 7} == BeamLang.run_source(source)
  end

  test "runs stdlib println" do
    source = """
    fn main() -> i32 {
        println("hello");
        return 0;
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source)
  end

  test "runs let-bound value" do
    source = """
    fn main() -> i32 {
        let value = 5;
        return value;
    }
    """

    assert {:ok, 5} == BeamLang.run_source(source)
  end

  test "supports mutable reassignment" do
    source = """
    fn main() -> i32 {
        let mut value = 1;
        value = 3;
        return value;
    }
    """

    assert {:ok, 3} == BeamLang.run_source(source)
  end

  test "supports void return" do
    source = """
    fn main() -> void {
        return;
    }
    """

    assert {:error, [error]} = BeamLang.run_source(source)
    assert error.message == "main must return i32, got void."
  end

  test "supports struct literal with type annotation" do
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

    assert {:ok, 1} == BeamLang.run_source(source)
  end

  test "supports struct field access" do
    source = """
    type User {
        id: i32,
        name: String
    }

    fn main() -> i32 {
        let user: User = { id = 2, name = "Peter" };
        return user->id;
    }
    """

    assert {:ok, 2} == BeamLang.run_source(source)
  end

  test "supports struct field assignment" do
    source = """
    type User {
        id: i32,
        name: String
    }

    fn main() -> i32 {
        let mut user: User = { id = 1, name = "Peter" };
        user->id = 3;
        return user->id;
    }
    """

    assert {:ok, 3} == BeamLang.run_source(source)
  end

  test "rejects non-i32 main" do
    source = """
    fn main() -> bool {
        return true;
    }
    """

    assert {:error, [error]} = BeamLang.run_source(source)
    assert error.message == "main must return i32, got bool."
  end

  test "runs match expression" do
    source = """
    fn main() -> i32 {
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
    fn main() -> i32 {
        let value = if (true) { 1; } else { 2; };
        return value;
    }
    """

    assert {:ok, 1} == BeamLang.run_source(source)
  end

  test "runs loop with break" do
    source = """
    fn main() -> i32 {
        loop { break; }
        return 0;
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source)
  end

  test "runs while with false condition" do
    source = """
    fn main() -> i32 {
        while (false) { break; }
        return 0;
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source)
  end

  test "runs for over string" do
    source = """
    fn main() -> i32 {
        for (item in "hi") { break; }
        return 0;
    }
    """

    assert {:ok, 0} == BeamLang.run_source(source)
  end
end
