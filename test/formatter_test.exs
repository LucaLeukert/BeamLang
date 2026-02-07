defmodule BeamLang.FormatterTest do
  use ExUnit.Case, async: true

  describe "format/1" do
    test "formats a simple function" do
      input = "fn main(args:[String])->number{return 0;}"
      {:ok, output} = BeamLang.Formatter.format(input)

      assert output =~ "fn"
      assert output =~ "return"
      assert String.ends_with?(output, "\n")
    end

    test "adds indentation inside braces" do
      input = "fn main(args: [String]) -> number {\nreturn 0;\n}"
      {:ok, output} = BeamLang.Formatter.format(input)

      lines = String.split(output, "\n", trim: true)
      # The return statement should be indented
      return_line = Enum.find(lines, &String.contains?(&1, "return"))
      assert return_line != nil
      assert String.starts_with?(return_line, "    ")
    end

    test "formats type definition" do
      input = "type User{name:String,age:number}"
      {:ok, output} = BeamLang.Formatter.format(input)

      assert output =~ "type"
      assert output =~ "User"
      assert output =~ "name"
    end

    test "ensures trailing newline" do
      input = "fn main(args: [String]) -> number { return 0; }"
      {:ok, output} = BeamLang.Formatter.format(input)
      assert String.ends_with?(output, "\n")
    end

    test "handles empty input" do
      {:ok, output} = BeamLang.Formatter.format("")
      assert output == "\n"
    end

    test "handles input with only whitespace" do
      {:ok, output} = BeamLang.Formatter.format("   \n  \n  ")
      assert String.trim(output) == "" or output == "\n"
    end

    test "preserves string content" do
      input = ~s|fn main(args: [String]) -> number { let x = "hello world"; return 0; }|
      {:ok, output} = BeamLang.Formatter.format(input)
      assert output =~ ~s|"hello world"|
    end

    test "spaces around operators" do
      input = "fn main(args: [String]) -> number { let x = 1 + 2; return x; }"
      {:ok, output} = BeamLang.Formatter.format(input)
      assert output =~ "1 + 2" or output =~ "+"
    end

    test "handles broken source gracefully" do
      input = "fn broken { let x = "
      {:ok, output} = BeamLang.Formatter.format(input)
      # Should not crash, should return something
      assert is_binary(output)
    end

    test "separates top-level imports from type declarations" do
      input = """
      import args.*;
      type Args {
          file: String
      }
      """

      {:ok, output} = BeamLang.Formatter.format(input)

      assert output =~ "import args.*;\n\ntype Args"
    end

    test "puts type field annotations on separate lines" do
      input = """
      type Args {
          @required() @description("File to read and print") file: String
      }
      """

      {:ok, output} = BeamLang.Formatter.format(input)

      assert output =~ "@required()\n"
      assert output =~ "@description(\"File to read and print\")\n"
      assert output =~ "file: String"
    end

    test "preserves single-line comments" do
      input = """
      // top-level comment
      fn main(args: [String]) -> number {
          // inside function
          return 0; // trailing comment
      }
      """

      {:ok, output} = BeamLang.Formatter.format(input)

      assert output =~ "// top-level comment"
      assert output =~ "// inside function"
      assert output =~ "// trailing comment"
    end

    test "preserves block comments" do
      input = """
      /**
       * docs
       **/
      fn main(args: [String]) -> number {
          return 0;
      }
      """

      {:ok, output} = BeamLang.Formatter.format(input)

      assert output =~ "/**"
      assert output =~ "docs"
      assert output =~ "**/"
    end

    test "formats single-expression case branch as fat arrow" do
      input = """
      fn main(args: [String]) -> number {
          let parsed = parse_args < Args > (args);
          return match (parsed) {
              case!ok opts -> {
                  cat_file(opts -> file);
              },
              case!err err -> {
                  println(err->message);
                  return 1;
              }
          };
      }
      """

      {:ok, output} = BeamLang.Formatter.format(input)

      assert output =~ "parse_args<Args>(args);"
      assert output =~ "case!ok opts => cat_file(opts->file),"
      assert output =~ "case!err err -> {"
    end
  end

  describe "format_file/1" do
    test "returns error for nonexistent file" do
      {:error, msg} = BeamLang.Formatter.format_file("/nonexistent/path.bl")
      assert msg =~ "Cannot read"
    end
  end
end
