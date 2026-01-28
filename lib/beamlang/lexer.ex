defmodule BeamLang.Lexer do
  @moduledoc """
  Converts BeamLang source text into a stream of tokens.
  """

  alias BeamLang.Token

  @keywords %{
    "fn" => :fn,
    "type" => :type_kw,
    "return" => :return,
    "let" => :let,
    "mut" => :mut,
    "match" => :match,
    "case" => :case,
    "if" => :if_kw,
    "guard" => :guard,
    "else" => :else_kw,
    "for" => :for_kw,
    "in" => :in_kw,
    "while" => :while_kw,
    "loop" => :loop_kw,
    "break" => :break_kw,
    "i32" => :type,
    "i64" => :type,
    "f32" => :type,
    "f64" => :type,
    "String" => :type,
    "void" => :type,
    "bool" => :type,
    "true" => :bool,
    "false" => :bool
  }

  @symbols %{
    ?( => :lparen,
    ?) => :rparen,
    ?{ => :lbrace,
    ?} => :rbrace,
    ?; => :semicolon,
    ?= => :equals,
    ?< => :lt,
    ?> => :gt,
    ?: => :colon,
    ?, => :comma
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

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<"->", rest::binary>>, file, offset, line, col, acc) do
    span = BeamLang.Span.new(file, offset, offset + 2)
    token = %Token{type: :arrow, value: "->", line: line, col: col, span: span}
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
        {:error, error("Unexpected '-'.", file, offset, line, col)}
    end
  end

  defp do_tokenize(<<"!", _rest::binary>>, file, offset, line, col, _acc) do
    {:error, error("Unexpected '!'.", file, offset, line, col)}
  end

  @spec do_tokenize(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [Token.t()]) ::
          {:ok, [Token.t()]} | {:error, BeamLang.Error.t()}
  defp do_tokenize(<<c, rest::binary>>, file, offset, line, col, acc) when c in ?0..?9 do
    {type, value, rest_after, width} = read_number(<<c, rest::binary>>, 1)
    span = BeamLang.Span.new(file, offset, offset + width)
    token = %Token{type: type, value: value, line: line, col: col, span: span}
    do_tokenize(rest_after, file, offset + width, line, col + width, [token | acc])
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
      <<".", rest2::binary>> ->
        {frac, rest3} = take_while(rest2, fn c -> c in ?0..?9 end)
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

  defp read_string(<<c, rest::binary>>, acc) do
    read_string(rest, acc <> <<c>>)
  end

  defp read_string(<<>>, _acc), do: {:error, "Unterminated string literal."}

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

  @spec error(binary(), binary(), non_neg_integer(), non_neg_integer(), non_neg_integer()) :: BeamLang.Error.t()
  defp error(message, file, offset, line, col) do
    span = BeamLang.Span.new(file, offset, offset + 1)
    %BeamLang.Error{kind: :lexer, message: message, span: span, notes: ["at line #{line}, col #{col}"]}
  end
end
