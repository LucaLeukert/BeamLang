defmodule BeamLang.SemanticTest do
  use ExUnit.Case, async: true

  alias BeamLang.{Lexer, Parser, Semantic}

  defp parse_program!(source, filename) do
    {:ok, tokens} = Lexer.tokenize(source, filename)
    {:ok, ast} = Parser.parse(tokens)
    ast
  end

  defp merge_programs(
         {:program, %{types: types1, enums: enums1, errors: errors1, functions: funcs1}},
         {:program, %{types: types2, enums: enums2, errors: errors2, functions: funcs2}}
       ) do
    {:program,
     %{
       module: nil,
       imports: [],
       types: types1 ++ types2,
       enums: enums1 ++ enums2,
       errors: errors1 ++ errors2,
       functions: funcs1 ++ funcs2,
       span: BeamLang.Span.new("<test>", 0, 0)
     }}
  end

  test "rejects internal function calls across non-stdlib files" do
    hidden_src = """
    internal fn hidden() -> number {
        return 1;
    }
    """

    caller_src = """
    fn caller() -> number {
        return hidden();
    }
    """

    hidden = parse_program!(hidden_src, "/tmp/beamlang_hidden.bl")
    caller = parse_program!(caller_src, "/tmp/beamlang_caller.bl")

    merged = merge_programs(hidden, caller)

    assert {:error, errors} = Semantic.validate(merged, require_main: false)
    assert Enum.any?(errors, fn err -> err.message =~ "Internal function cannot be called" end)
  end

  test "allows internal function calls across stdlib files" do
    hidden_src = """
    internal fn hidden() -> number {
        return 1;
    }
    """

    caller_src = """
    fn caller() -> number {
        return hidden();
    }
    """

    hidden = parse_program!(hidden_src, "/repo/stdlib/core/hidden.bl")
    caller = parse_program!(caller_src, "/repo/stdlib/core/caller.bl")

    merged = merge_programs(hidden, caller)

    assert {:ok, _} = Semantic.validate(merged, require_main: false)
  end
end
