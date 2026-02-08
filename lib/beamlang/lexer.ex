defmodule BeamLang.Lexer do
  @moduledoc """
  Converts BeamLang source text into a stream of tokens.
  """

  alias BeamLang.Token

  @keywords %{
    "fn" => :fn,
    "type" => :type_kw,
    "enum" => :enum_kw,
    "error" => :error_kw,
    "import" => :import_kw,
    "export" => :export_kw,
    "internal" => :internal_kw,
    "operator" => :operator_kw,
    "as" => :as_kw,
    "return" => :return,
    "let" => :let,
    "mut" => :mut,
    "match" => :match,
    "case" => :case,
    "if" => :if_kw,
    "async" => :async_kw,
    "await" => :await_kw,
    "guard" => :guard,
    "else" => :else_kw,
    "for" => :for_kw,
    "in" => :in_kw,
    "while" => :while_kw,
    "loop" => :loop_kw,
    "break" => :break_kw,
    "number" => :type,
    "String" => :type,
    "char" => :type,
    "void" => :type,
    "bool" => :type,
    "any" => :type,
    "true" => :bool,
    "false" => :bool
  }

  @symbols %{
    ?( => :lparen,
    ?) => :rparen,
    ?{ => :lbrace,
    ?} => :rbrace,
    ?[ => :lbracket,
    ?] => :rbracket,
    ?; => :semicolon,
    ?. => :dot,
    ?= => :equals,
    ?< => :lt,
    ?> => :gt,
    ?+ => :plus,
    ?* => :star,
    ?/ => :slash,
    ?% => :percent,
    ?? => :question,
    ?: => :colon,
    ?, => :comma,
    ?@ => :at
  }

  @spec tokenize(binary()) :: {:ok, [Token.t()]} | {:error, map()}
  @spec tokenize(binary(), binary()) :: {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  def tokenize(source, filename) when is_binary(source) and is_binary(filename) do
    do_tokenize(source, filename, 0, 1, 1, [])
  end

  @spec tokenize(binary()) :: {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  def tokenize(source) when is_binary(source) do
    tokenize(source, "<source>")
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<>>, _file, _offset, _line, _col, acc) do
    {:ok, Enum.reverse(acc)}
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"\n", rest::binary>>, file, offset, line, _col, acc) do
    do_tokenize(rest, file, offset + 1, line + 1, 1, acc)
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<c, rest::binary>>, file, offset, line, col, acc) when c in [?\t, ?\r, ?\n, ?\s] do
    do_tokenize(rest, file, offset + 1, line, col + 1, acc)
  end

  # Single-line comment: //
  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"//", rest::binary>>, file, offset, line, _col, acc) do
    {rest_after, width} = skip_line_comment(rest, 2)
    do_tokenize(rest_after, file, offset + width, line, 1, acc)
  end

  # Multi-line comment: /** **/
  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"/**", rest::binary>>, file, offset, line, col, acc) do
    case skip_block_comment(rest, 3, line) do
      {:ok, rest_after, width, new_line} ->
        do_tokenize(rest_after, file, offset + width, new_line, col + width, acc)

      {:error, message} ->
        {:error, error(message, file, offset, line, col)}
    end
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"->", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :arrow, value: "->", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 2, line, col + 2, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"::", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :double_colon, value: "::", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 2, line, col + 2, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"==", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :eq_eq, value: "==", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 2, line, col + 2, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"!=", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :neq, value: "!=", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 2, line, col + 2, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"!", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 1)
    token = %Token{type: :bang, value: "!", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 1, line, col + 1, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"<=", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :lte, value: "<=", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 2, line, col + 2, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<">=", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :gte, value: ">=", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 2, line, col + 2, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"=>", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :fat_arrow, value: "=>", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 2, line, col + 2, [token | acc])
  end

  # Compound assignment operators
  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"+=", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :plus_eq, value: "+=", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 2, line, col + 2, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"-=", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :minus_eq, value: "-=", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 2, line, col + 2, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"*=", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :star_eq, value: "*=", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 2, line, col + 2, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"/=", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :slash_eq, value: "/=", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 2, line, col + 2, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"%=", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :percent_eq, value: "%=", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 2, line, col + 2, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"-", rest::binary>>, file, offset, line, col, acc) do
    case rest do
      <<next, _::binary>> when next in ?0..?9 ->
        {type, value, rest_after, width} = read_number(rest, -1)
        span = BeamLang.Span.new(file, offset, offset + width + 1)
        token = %Token{type: type, value: value, line: line, col: col, span: span}
        do_tokenize(rest_after, file, offset + width + 1, line, col + width + 1, [token | acc])

      _ ->
        span = BeamLang.Span.new(file, offset, offset + 1)
        token = %Token{type: :minus, value: "-", line: line, col: col, span: span}
        do_tokenize(rest, file, offset + 1, line, col + 1, [token | acc])
    end
  end


  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<c, rest::binary>>, file, offset, line, col, acc) when c in ?0..?9 do
    {type, value, rest_after, width} = read_number(<<c, rest::binary>>, 1)
    span = BeamLang.Span.new(file, offset, offset + width)
    token = %Token{type: type, value: value, line: line, col: col, span: span}
    do_tokenize(rest_after, file, offset + width, line, col + width, [token | acc])
  end

  defp do_tokenize(<<"\"\"\"", rest::binary>>, file, offset, line, col, acc) do
    case read_multiline_string(rest, "", line, col + 3, 3) do
      {:ok, value, rest_after, width, new_line, new_col} ->
        span = BeamLang.Span.new(file, offset, offset + width)
        token = %Token{type: :string, value: value, line: line, col: col, span: span}
        do_tokenize(rest_after, file, offset + width, new_line, new_col, [token | acc])

      {:error, message} ->
        {:error, error(message, file, offset, line, col)}
    end
  end

  defp do_tokenize(<<"\"", rest::binary>>, file, offset, line, col, acc) do
    case read_string(rest, "") do
      {:ok, value, rest_after, width} ->
        span = BeamLang.Span.new(file, offset, offset + width + 2)
        token = %Token{type: :string, value: value, line: line, col: col, span: span}
        do_tokenize(rest_after, file, offset + width + 2, line, col + width + 2, [token | acc])

      {:error, message} ->
        {:error, error(message, file, offset, line, col)}
    end
  end

  defp do_tokenize(<<"'", rest::binary>>, file, offset, line, col, acc) do
    case read_char(rest) do
      {:ok, value, rest_after, width} ->
        span = BeamLang.Span.new(file, offset, offset + width + 2)
        token = %Token{type: :char, value: value, line: line, col: col, span: span}
        do_tokenize(rest_after, file, offset + width + 2, line, col + width + 2, [token | acc])

      {:error, message} ->
        {:error, error(message, file, offset, line, col)}
    end
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<c, rest::binary>>, file, offset, line, col, acc) when c in ?a..?z or c in ?A..?Z or c == ?_ do
    {ident, rest_after, width} = read_identifier(<<c, rest::binary>>)

    {type, value} =
      case Map.get(@keywords, ident) do
        nil -> {:identifier, ident}
        :type -> {:type, ident}
        kw -> {kw, ident}
      end

    span = BeamLang.Span.new(file, offset, offset + width)
    token = %Token{type: type, value: value, line: line, col: col, span: span}
    do_tokenize(rest_after, file, offset + width, line, col + width, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"..", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :dotdot, value: "..", line: line, col: col, span: span}
    do_tokenize(rest, file, offset + 2, line, col + 2, [token | acc])
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<c, rest::binary>>, file, offset, line, col, acc) do
    case Map.get(@symbols, c) do
      nil ->
        {:error, error("Unexpected character '#{<<c>>}'.", file, offset, line, col)}

      type ->
        span = BeamLang.Span.new(file, offset, offset + 1)
        token = %Token{type: type, value: <<c>>, line: line, col: col, span: span}
        do_tokenize(rest, file, offset + 1, line, col + 1, [token | acc])
    end
  end

  @spec read_number(binary(), integer()) ::
          {:integer | :float, number(), binary(), non_neg_integer()}
  defp read_number(binary, sign) do
    {digits, rest} = take_while(binary, fn c -> c in ?0..?9 end)

    case rest do
      # Check for float: must be "." followed by digit, not ".." (range)
      <<".", next, rest2::binary>> when next in ?0..?9 ->
        {frac, rest3} = take_while(<<next, rest2::binary>>, fn c -> c in ?0..?9 end)
        number = String.to_float("#{digits}.#{frac}") * sign
        width = byte_size(digits) + 1 + byte_size(frac)
        {:float, number, rest3, width}

      _ ->
        value = sign * String.to_integer(digits)
        {:integer, value, rest, byte_size(digits)}
    end
  end

  @spec read_string(binary(), binary()) ::
          {:ok, binary(), binary(), non_neg_integer()} | {:error, binary()}
  defp read_string(<<"\"", rest::binary>>, acc), do: {:ok, acc, rest, byte_size(acc)}

  defp read_string(<<c, _rest::binary>>, _acc) when c in [?\n, ?\r] do
    {:error, "Unterminated string literal."}
  end

  # Handle escape sequences
  defp read_string(<<"\\", esc, rest::binary>>, acc) do
    char = case esc do
      ?n -> "\n"
      ?r -> "\r"
      ?t -> "\t"
      ?\\ -> "\\"
      ?" -> "\""
      ?$ -> "$"
      other -> <<other>>
    end
    read_string(rest, acc <> char)
  end

  defp read_string(<<c, rest::binary>>, acc) do
    read_string(rest, acc <> <<c>>)
  end

  defp read_string(<<>>, _acc), do: {:error, "Unterminated string literal."}

  @spec read_multiline_string(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer()) ::
          {:ok, binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer()}
          | {:error, binary()}
  defp read_multiline_string(<<"\"\"\"", rest::binary>>, acc, line, col, width) do
    total_width = width + 3
    {:ok, acc, rest, total_width, line, col + 3}
  end

  defp read_multiline_string(<<"\n", rest::binary>>, acc, line, _col, width) do
    read_multiline_string(rest, acc <> "\n", line + 1, 1, width + 1)
  end

  defp read_multiline_string(<<c, rest::binary>>, acc, line, col, width) do
    read_multiline_string(rest, acc <> <<c>>, line, col + 1, width + 1)
  end

  defp read_multiline_string(<<>>, _acc, _line, _col, _width) do
    {:error, "Unterminated multiline string literal."}
  end

  @spec read_char(binary()) :: {:ok, integer(), binary(), non_neg_integer()} | {:error, binary()}
  defp read_char(<<"\\", esc, "'", rest::binary>>) do
    value =
      case esc do
        ?n -> ?\n
        ?r -> ?\r
        ?t -> ?\t
        ?\\ -> ?\\
        ?' -> ?'
        other -> other
      end

    {:ok, value, rest, 2}
  end

  defp read_char(<<c, "'", rest::binary>>), do: {:ok, c, rest, 1}
  defp read_char(_), do: {:error, "Invalid char literal."}

  @spec read_identifier(binary()) :: {binary(), binary(), non_neg_integer()}
  defp read_identifier(binary) do
    {ident, rest} =
      take_while(binary, fn c -> c in ?a..?z or c in ?A..?Z or c in ?0..?9 or c == ?_ end)

    {ident, rest, byte_size(ident)}
  end

  @spec take_while(binary(), (integer() -> boolean())) :: {binary(), binary()}
  defp take_while(binary, predicate) do
    take_while(binary, predicate, "")
  end

  @spec take_while(binary(), (integer() -> boolean()), binary()) :: {binary(), binary()}
  defp take_while(<<c, rest::binary>>, predicate, acc) do
    if predicate.(c) do
      take_while(rest, predicate, acc <> <<c>>)
    else
      {acc, <<c, rest::binary>>}
    end
  end

  @spec take_while(binary(), (integer() -> boolean()), binary()) :: {binary(), binary()}
  defp take_while(<<>>, _predicate, acc) do
    {acc, <<>>}
  end

  # Skip single-line comment until newline or EOF
  @spec skip_line_comment(binary(), non_neg_integer()) :: {binary(), non_neg_integer()}
  defp skip_line_comment(<<"\n", rest::binary>>, width), do: {rest, width + 1}
  defp skip_line_comment(<<>>, width), do: {<<>>, width}
  defp skip_line_comment(<<_, rest::binary>>, width), do: skip_line_comment(rest, width + 1)

  # Skip multi-line comment until **/ or EOF (error)
  @spec skip_block_comment(binary(), non_neg_integer(), non_neg_integer()) ::
          {:ok, binary(), non_neg_integer(), non_neg_integer()} | {:error, binary()}
  defp skip_block_comment(<<"**/", rest::binary>>, width, line), do: {:ok, rest, width + 3, line}
  defp skip_block_comment(<<>>, _width, _line), do: {:error, "Unterminated block comment."}
  defp skip_block_comment(<<"\n", rest::binary>>, width, line), do: skip_block_comment(rest, width + 1, line + 1)
  defp skip_block_comment(<<_, rest::binary>>, width, line), do: skip_block_comment(rest, width + 1, line)

  @spec error(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer()) :: BeamLang.Error.t()
  defp error(message, file, offset, line, col) do
    span = BeamLang.Span.new(file, offset, offset + 1)
    %BeamLang.Error{kind: :lexer, message: message, span: span, notes: ["at line #{line}, col #{col}"]}
  end
end
