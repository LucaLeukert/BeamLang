defmodule BeamLang.Formatter do
  @moduledoc """
  Source code formatter for BeamLang.

  Works at the token level for maximum robustness – can format files
  that don't fully parse. Produces canonical whitespace, indentation
  (4 spaces per level), and trailing newlines.
  """

  @indent_size 4

  # ---- Public API --------------------------------------------------------

  @spec format(binary()) :: {:ok, binary()} | {:error, binary()}
  def format(source) when is_binary(source) do
    if contains_comment?(source) do
      # Current formatter works token-by-token, but lexer drops comments.
      # Preserve source to avoid deleting comments.
      {:ok, ensure_trailing_newline(source)}
    else
      path = "<format>"

      case BeamLang.Lexer.tokenize(source, path) do
        {:ok, tokens} ->
          formatted = format_tokens(tokens, source)
          {:ok, formatted}

        {:error, _} ->
          # Fall back to simple whitespace normalisation when the source
          # can't even be tokenised.
          {:ok, normalize_whitespace(source)}
      end
    end
  end

  @spec format_file(binary()) :: {:ok, binary()} | {:error, binary()}
  def format_file(path) when is_binary(path) do
    case File.read(path) do
      {:ok, source} -> format(source)
      {:error, reason} -> {:error, "Cannot read #{path}: #{inspect(reason)}"}
    end
  end

  # ---- Token-level formatter ---------------------------------------------

  defp format_tokens(tokens, _source) do
    tokens
    |> strip_eof()
    |> BeamLang.Linter.autocorrect_tokens()
    |> do_format(0, :line_start, [])
    |> IO.iodata_to_binary()
    |> ensure_trailing_newline()
  end

  # Remove :eof tokens (they carry no source text)
  defp strip_eof(tokens) do
    Enum.reject(tokens, fn %BeamLang.Token{type: type} -> type == :eof end)
  end

  # `state` tracks where we are on the current line:
  #   :line_start  – haven't emitted anything on this line yet
  #   :after_token – just emitted a token (space before next)
  #   :after_open  – just emitted ( or [ (no space before next)
  defp do_format([], _indent, _state, acc), do: Enum.reverse(acc)

  # ---- Closing braces: dedent BEFORE emitting ---------------------------
  defp do_format([%BeamLang.Token{type: :rbrace} = tok | rest], indent, _state, acc) do
    new_indent = max(indent - 1, 0)
    acc = emit_newline_indent(acc, new_indent)
    acc = [token_text(tok) | acc]

    # After `}` at indent 0, if the next token starts a new top-level declaration,
    # add a blank line. If it's `else`, stay on same line.
    case rest do
      # `} else` - stay on same line
      [%BeamLang.Token{type: type} | _]
      when type in [:else_kw, :else] ->
        do_format(rest, new_indent, :after_token, acc)

      # `},` or `};` or `)` - trailing punctuation stays on same line as }
      [%BeamLang.Token{type: type} | _]
      when type in [:comma, :semicolon, :rparen] ->
        do_format(rest, new_indent, :after_token, acc)

      # Top-level declarations get a blank line separator
      [%BeamLang.Token{type: type} | _]
      when new_indent == 0 and
             type in [:fn, :fn_kw, :type_kw, :error_kw, :export_kw, :export, :import_kw, :import] ->
        do_format(rest, new_indent, :line_start, ["\n\n" | acc])

      _ ->
        # After } inside a function, start a new line for the next statement
        do_format(rest, new_indent, :line_start, ["\n" | acc])
    end
  end

  # ---- Opening braces: emit, then indent --------------------------------
  defp do_format([%BeamLang.Token{type: :lbrace} = tok | rest], indent, state, acc) do
    acc =
      case state do
        :line_start -> [indent_str(indent) | acc]
        :after_open -> acc
        :after_token -> [" " | acc]
      end

    acc = [token_text(tok) | acc]
    do_format(rest, indent + 1, :line_start, ["\n" | acc])
  end

  # ---- Semicolons: emit then newline ------------------------------------
  defp do_format([%BeamLang.Token{type: :semicolon} = tok | rest], indent, _state, acc) do
    acc = [token_text(tok) | acc]

    case rest do
      [%BeamLang.Token{type: type} | _]
      when indent == 0 and
             type in [
               :fn,
               :fn_kw,
               :type_kw,
               :error_kw,
               :export_kw,
               :export,
               :internal_kw,
               :internal
             ] ->
        # Separate top-level import/export statements from declarations.
        do_format(rest, indent, :line_start, ["\n\n" | acc])

      _ ->
        do_format(rest, indent, :line_start, ["\n" | acc])
    end
  end

  # ---- Commas: stay on same line but add space after --------------------
  defp do_format([%BeamLang.Token{type: :comma} = tok | rest], indent, _state, acc) do
    acc = [token_text(tok) | acc]

    case rest do
      [%BeamLang.Token{type: :rbrace} | _] ->
        # Don't add trailing space before closing brace
        do_format(rest, indent, :after_token, acc)

      # If the next token starts a block-like construct (case in match),
      # put it on a new line
      [%BeamLang.Token{type: type} | _] when type in [:case] ->
        do_format(rest, indent, :line_start, ["\n" | acc])

      _ ->
        do_format(rest, indent, :after_open, [" " | acc])
    end
  end

  # ---- Arrow (`->`) with spaces around ----------------------------------
  # Member/method access should not have surrounding spaces.
  defp do_format(
         [%BeamLang.Token{type: :arrow} = tok, %BeamLang.Token{type: :identifier} = next | rest],
         indent,
         :after_token,
         [prev | _] = acc
       )
       when is_binary(prev) do
    if identifier_like_text?(prev) do
      acc = [token_text(tok) | acc]
      do_format([next | rest], indent, :after_open, acc)
    else
      acc = [" " | acc]
      acc = [token_text(tok) | acc]
      do_format([next | rest], indent, :after_open, [" " | acc])
    end
  end

  defp do_format([%BeamLang.Token{type: :arrow} = tok | rest], indent, state, acc) do
    acc =
      case state do
        :line_start -> [indent_str(indent) | acc]
        :after_open -> acc
        :after_token -> [" " | acc]
      end

    acc = [token_text(tok) | acc]
    do_format(rest, indent, :after_open, [" " | acc])
  end

  # ---- Generic angle brackets: no extra spaces ---------------------------
  defp do_format(
         [%BeamLang.Token{type: :lt} = tok | rest],
         indent,
         :after_token,
         [prev | _] = acc
       )
       when is_binary(prev) do
    if identifier_like_text?(prev) and generic_arg_list?(rest) do
      acc = [token_text(tok) | acc]
      do_format(rest, indent, :after_open, acc)
    else
      acc = [" " | acc]
      acc = [token_text(tok) | acc]
      do_format(rest, indent, :after_open, [" " | acc])
    end
  end

  defp do_format([%BeamLang.Token{type: :gt} = tok | rest], indent, _state, acc) do
    if generic_close_context?(acc, rest) do
      acc = [token_text(tok) | acc]
      do_format(rest, indent, :after_token, acc)
    else
      acc = [token_text(tok) | acc]
      do_format(rest, indent, :after_open, [" " | acc])
    end
  end

  # ---- Colon: space after (in type annotations) -------------------------
  defp do_format([%BeamLang.Token{type: :colon} = tok | rest], indent, _state, acc) do
    acc = [token_text(tok) | acc]
    do_format(rest, indent, :after_open, [" " | acc])
  end

  # ---- Star/bang/question in non-operator context -------------------------
  # Star after dot (import globs): no space
  defp do_format([%BeamLang.Token{type: :star} = tok | rest], indent, :after_open, acc) do
    acc = [token_text(tok) | acc]
    do_format(rest, indent, :after_token, acc)
  end

  # Bang/question after case keyword (case!ok, case?some): no space
  defp do_format([%BeamLang.Token{type: type} = tok | rest], indent, _state, [prev | _] = acc)
       when type in [:bang, :question] and prev in ["case"] do
    acc = [token_text(tok) | acc]
    do_format(rest, indent, :after_open, acc)
  end

  # ---- Binary operators: spaces around ----------------------------------
  defp do_format([%BeamLang.Token{type: type} = tok | rest], indent, state, acc)
       when type in [
              :eq,
              :neq,
              :lte,
              :gte,
              :lt,
              :gt,
              :plus,
              :minus,
              :star,
              :slash,
              :modulo,
              :percent,
              :and_kw,
              :or_kw,
              :and_op,
              :or_op,
              :double_eq,
              :not_eq,
              :eq_eq,
              :not_eq,
              :plus_eq,
              :minus_eq,
              :star_eq,
              :slash_eq,
              :concat,
              :assign,
              :equals
            ] do
    acc =
      case state do
        :line_start -> [indent_str(indent) | acc]
        :after_open -> acc
        :after_token -> [" " | acc]
      end

    acc = [token_text(tok) | acc]
    do_format(rest, indent, :after_open, [" " | acc])
  end

  # ---- Parens/brackets: no extra space inside -----------------------------
  defp do_format([%BeamLang.Token{type: :lparen} = tok | rest], indent, state, acc) do
    # Add space before ( when preceded by a control flow keyword
    needs_space =
      case acc do
        [prev | _] when prev in ["if", "while", "match", "for", "guard"] -> true
        _ -> false
      end

    acc =
      case state do
        :line_start -> [indent_str(indent) | acc]
        :after_open -> acc
        :after_token -> if needs_space, do: [" " | acc], else: acc
      end

    acc = [token_text(tok) | acc]
    do_format(rest, indent, :after_open, acc)
  end

  defp do_format([%BeamLang.Token{type: :rparen} = tok | rest], indent, _state, acc) do
    acc = [token_text(tok) | acc]
    do_format(rest, indent, :after_token, acc)
  end

  defp do_format([%BeamLang.Token{type: :lbracket} = tok | rest], indent, state, acc) do
    acc =
      case state do
        :line_start -> [indent_str(indent) | acc]
        :after_open -> acc
        :after_token -> acc
      end

    acc = [token_text(tok) | acc]
    do_format(rest, indent, :after_open, acc)
  end

  defp do_format([%BeamLang.Token{type: :rbracket} = tok | rest], indent, _state, acc) do
    acc = [token_text(tok) | acc]
    do_format(rest, indent, :after_token, acc)
  end

  # ---- Dot: no spaces around (field access, imports) ----------------------
  defp do_format([%BeamLang.Token{type: :dot} = tok | rest], indent, _state, acc) do
    acc = [token_text(tok) | acc]
    do_format(rest, indent, :after_open, acc)
  end

  # ---- At sign (@): no space after (decorators) --------------------------
  defp do_format([%BeamLang.Token{type: :at} = tok | rest], indent, state, acc) do
    acc =
      case state do
        :line_start ->
          [indent_str(indent) | acc]

        :after_open ->
          acc

        :after_token ->
          if indent > 0 do
            # Keep field annotations one-per-line in type bodies.
            emit_newline_indent(acc, indent)
          else
            [" " | acc]
          end
      end

    acc = [token_text(tok) | acc]
    do_format(rest, indent, :after_open, acc)
  end

  # Field name after annotation call should start on a new line.
  defp do_format(
         [%BeamLang.Token{type: :identifier} = tok | rest],
         indent,
         :after_token,
         [")" | _] = acc
       )
       when indent > 0 do
    if decorator_on_current_line?(acc) do
      acc = emit_newline_indent(acc, indent)
      acc = [token_text(tok) | acc]
      do_format(rest, indent, :after_token, acc)
    else
      acc = [" " | acc]
      acc = [token_text(tok) | acc]
      do_format(rest, indent, :after_token, acc)
    end
  end

  # ---- General token rule ------------------------------------------------
  defp do_format([tok | rest], indent, state, acc) do
    acc =
      case state do
        :line_start -> [indent_str(indent) | acc]
        :after_open -> acc
        :after_token -> [spacing_before(tok, acc) | acc]
      end

    acc = [token_text(tok) | acc]
    do_format(rest, indent, :after_token, acc)
  end

  # ---- Helpers -----------------------------------------------------------

  defp identifier_like_text?(text) when is_binary(text) do
    String.match?(text, ~r/^[A-Za-z_][A-Za-z0-9_]*$/)
  end

  defp generic_arg_list?(tokens) do
    case find_top_level_generic_close(tokens, 0) do
      {:ok, rest} -> generic_close_delimiter?(rest)
      :error -> false
    end
  end

  defp find_top_level_generic_close([], _depth), do: :error

  defp find_top_level_generic_close([%BeamLang.Token{type: :lt} | rest], depth) do
    find_top_level_generic_close(rest, depth + 1)
  end

  defp find_top_level_generic_close([%BeamLang.Token{type: :gt} | rest], 0), do: {:ok, rest}

  defp find_top_level_generic_close([%BeamLang.Token{type: :gt} | rest], depth) do
    find_top_level_generic_close(rest, depth - 1)
  end

  defp find_top_level_generic_close([_ | rest], depth) do
    find_top_level_generic_close(rest, depth)
  end

  defp generic_close_context?(acc, rest) do
    case acc do
      [prev | _] when is_binary(prev) ->
        (identifier_like_text?(prev) or prev in ["]", ")", ">"]) and
          generic_close_delimiter?(rest)

      _ ->
        false
    end
  end

  defp generic_close_delimiter?([%BeamLang.Token{type: type} | _])
       when type in [
              :lparen,
              :comma,
              :rparen,
              :semicolon,
              :arrow,
              :fat_arrow,
              :equals,
              :assign,
              :lbrace,
              :rbrace,
              :colon
            ] do
    true
  end

  defp generic_close_delimiter?([]), do: true
  defp generic_close_delimiter?(_), do: false

  defp spacing_before(%BeamLang.Token{type: type}, _acc) when type in [:dot], do: ""

  defp spacing_before(%BeamLang.Token{type: :identifier, value: value}, acc)
       when value in ["else"] do
    # `else` after `}` should be ` else` not newline
    case acc do
      ["}" | _] -> " "
      _ -> " "
    end
  end

  defp spacing_before(_tok, _acc), do: " "

  defp decorator_on_current_line?(acc) do
    acc
    |> Enum.take_while(&(&1 != "\n"))
    |> Enum.any?(&(&1 == "@"))
  end

  defp token_text(%BeamLang.Token{type: type, value: value}) do
    case type do
      :identifier -> value
      :type -> value
      :integer -> to_string(value)
      :float -> to_string(value)
      :string -> "\"#{escape_string(value)}\""
      :char -> "'#{value}'"
      :bool -> to_string(value)
      :fn -> "fn"
      :fn_kw -> "fn"
      :let -> "let"
      :let_kw -> "let"
      :mut -> "mut"
      :mut_kw -> "mut"
      :return -> "return"
      :return_kw -> "return"
      :if -> "if"
      :if_kw -> "if"
      :else -> "else"
      :else_kw -> "else"
      :while -> "while"
      :while_kw -> "while"
      :for -> "for"
      :for_kw -> "for"
      :in -> "in"
      :in_kw -> "in"
      :match -> "match"
      :match_kw -> "match"
      :case -> "case"
      :case_kw -> "case"
      :loop -> "loop"
      :loop_kw -> "loop"
      :break -> "break"
      :break_kw -> "break"
      :continue -> "continue"
      :continue_kw -> "continue"
      :guard -> "guard"
      :guard_kw -> "guard"
      :import -> "import"
      :import_kw -> "import"
      :type_kw -> "type"
      :error -> "error"
      :error_kw -> "error"
      :export -> "export"
      :export_kw -> "export"
      :internal -> "internal"
      :internal_kw -> "internal"
      :true_val -> "true"
      :true_kw -> "true"
      :false_val -> "false"
      :false_kw -> "false"
      :none -> "none"
      :none_kw -> "none"
      :and_op -> "and"
      :and_kw -> "and"
      :or_op -> "or"
      :or_kw -> "or"
      :not_op -> "not"
      :not_kw -> "not"
      :lparen -> "("
      :rparen -> ")"
      :lbrace -> "{"
      :rbrace -> "}"
      :lbracket -> "["
      :rbracket -> "]"
      :semicolon -> ";"
      :colon -> ":"
      :comma -> ","
      :dot -> "."
      :arrow -> "->"
      :fat_arrow -> "=>"
      :equals -> "="
      :assign -> "="
      :plus -> "+"
      :minus -> "-"
      :star -> "*"
      :slash -> "/"
      :modulo -> "%"
      :percent -> "%"
      :eq -> "=="
      :eq_eq -> "=="
      :neq -> "!="
      :lt -> "<"
      :gt -> ">"
      :lte -> "<="
      :gte -> ">="
      :double_eq -> "=="
      :not_eq -> "!="
      :plus_eq -> "+="
      :minus_eq -> "-="
      :star_eq -> "*="
      :slash_eq -> "/="
      :concat -> "++"
      :at -> "@"
      :bang -> "!"
      :question -> "?"
      :pipe -> "|"
      :double_colon -> "::"
      :case_ok -> "case!ok"
      :case_err -> "case!err"
      :case_some -> "case?some"
      :case_none -> "case?none"
      _ -> to_string(value)
    end
  end

  defp escape_string(s) when is_binary(s) do
    s
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
    |> String.replace("\n", "\\n")
    |> String.replace("\t", "\\t")
    |> String.replace("\r", "\\r")
  end

  defp indent_str(level) when level <= 0, do: ""
  defp indent_str(level), do: String.duplicate(" ", level * @indent_size)

  defp emit_newline_indent(acc, indent) do
    # Only emit newline if we're not already at line start
    case acc do
      ["\n" | _] -> [indent_str(indent) | acc]
      _ -> [indent_str(indent) | ["\n" | acc]]
    end
  end

  defp ensure_trailing_newline(""), do: "\n"

  defp ensure_trailing_newline(text) do
    if String.ends_with?(text, "\n") do
      text
    else
      text <> "\n"
    end
  end

  # ---- Fallback: simple whitespace normalisation -------------------------

  defp normalize_whitespace(source) do
    source
    |> String.split("\n")
    |> Enum.map(&String.trim_trailing/1)
    |> Enum.join("\n")
    |> ensure_trailing_newline()
  end

  defp contains_comment?(source) when is_binary(source) do
    String.contains?(source, "//") or String.contains?(source, "/**")
  end
end
