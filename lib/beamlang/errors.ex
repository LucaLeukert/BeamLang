defmodule BeamLang.Errors do
  @moduledoc """
  Error collection and pretty-printing helpers.
  """

  alias BeamLang.Error

  @spec format(Error.t() | [Error.t()], binary()) :: binary()
  def format(errors, source) when is_list(errors) do
    errors
    |> Enum.map(&format(&1, source))
    |> Enum.join("\n")
  end

  def format(%Error{} = error, source) when is_binary(source) do
    {line_idx, col, lines} = line_index(source, error.span.start)
    {ctx_start, ctx_end} = context_range(lines, line_idx)

    header = "#{kind_label(error.kind)} error: #{error.message}"
    location = " --> #{error.span.file_id}:#{line_idx + 1}:#{col}"
    gutter = "  |"

    {body_lines, caret_line} =
      build_context(
        lines,
        ctx_start,
        ctx_end,
        line_idx,
        col,
        max_span_width(error.span, source)
      )

    notes =
      case error.notes do
        [] -> ""
        notes_list -> "\n" <> Enum.map_join(notes_list, "\n", &("note: " <> &1))
      end

    Enum.join([header, location, gutter] ++ body_lines ++ [caret_line], "\n") <> notes
  end

  @spec kind_label(Error.kind()) :: binary()
  defp kind_label(:lexer), do: "Lexer"
  defp kind_label(:parser), do: "Parser"
  defp kind_label(:type), do: "Type"
  defp kind_label(_), do: "Compiler"

  @spec line_index(binary(), non_neg_integer()) ::
          {non_neg_integer(), non_neg_integer(), [binary()]}
  defp line_index(source, offset) do
    lines = String.split(source, "\n", trim: false)
    {line_num, line_start} = find_line_start(lines, offset, 0, 0)
    col = offset - line_start + 1
    {line_num, col, lines}
  end

  @spec find_line_start([binary()], non_neg_integer(), non_neg_integer(), non_neg_integer()) ::
          {non_neg_integer(), non_neg_integer()}
  defp find_line_start([], _offset, line_num, line_start), do: {line_num, line_start}

  defp find_line_start([line | rest], offset, line_num, line_start) do
    line_len = byte_size(String.trim_trailing(line, "\r"))

    if offset <= line_start + line_len do
      {line_num, line_start}
    else
      find_line_start(rest, offset, line_num + 1, line_start + line_len + 1)
    end
  end

  @spec caret_line(non_neg_integer(), non_neg_integer()) :: binary()
  defp caret_line(col, width) do
    prefix = String.duplicate(" ", max(col - 1, 0))
    carets = String.duplicate("^", max(width, 1))
    prefix <> carets
  end

  @spec max_span_width(BeamLang.Span.t(), binary()) :: non_neg_integer()
  defp max_span_width(span, source) do
    span_width = max(span.end - span.start, 1)
    line_end = span_end_of_line(source, span.start)
    min(span_width, max(line_end - span.start, 1))
  end

  @spec span_end_of_line(binary(), non_neg_integer()) :: non_neg_integer()
  defp span_end_of_line(source, offset) do
    {line_idx, line_start, lines} =
      case line_index(source, offset) do
        {idx, col, lines} -> {idx, offset - col + 1, lines}
      end

    line_text = String.trim_trailing(Enum.at(lines, line_idx, ""), "\r")
    line_start + byte_size(line_text)
  end

  @spec context_range([binary()], non_neg_integer()) :: {non_neg_integer(), non_neg_integer()}
  defp context_range(lines, line_idx) do
    start_idx = find_function_head(lines, line_idx)
    {start_idx, line_idx}
  end

  @spec find_function_head([binary()], non_neg_integer()) :: non_neg_integer()
  defp find_function_head(lines, line_idx) do
    if line_idx == 0 do
      0
    else
      Enum.reduce_while(Range.new(line_idx, 0, -1), line_idx, fn idx, _acc ->
        line = String.trim_leading(Enum.at(lines, idx, ""))
        if String.starts_with?(line, "fn ") do
          {:halt, idx}
        else
          {:cont, idx}
        end
      end)
    end
  end

  @spec build_context([binary()], non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()) ::
          {[binary()], binary()}
  defp build_context(lines, start_idx, end_idx, error_idx, col, span_width) do
    {body_lines, caret_col} =
      Enum.reduce(start_idx..end_idx, {[], col}, fn idx, {acc, caret_col} ->
        line = String.trim_trailing(Enum.at(lines, idx, ""), "\r")
    {trimmed, prefix_offset} = trim_line(line, col, idx == error_idx)
        caret_col = if idx == error_idx, do: max(col - prefix_offset, 1), else: caret_col
        {[ "#{idx + 1} | #{trimmed}" | acc], caret_col}
      end)

    body_lines = Enum.reverse(body_lines)
    caret = caret_line(caret_col, span_width)
    caret_line = "  | #{caret}"
    {body_lines, caret_line}
  end

  @spec trim_line(binary(), non_neg_integer(), boolean()) :: {binary(), non_neg_integer()}
  defp trim_line(line, col, is_error_line) do
    max_width = 120

    if byte_size(line) <= max_width do
      {line, 0}
    else
      if is_error_line do
        start = max(col - 20, 0)
        trimmed = binary_slice(line, start, max_width)
        prefix = if start > 0, do: "…", else: ""
        suffix = if byte_size(line) > start + max_width, do: "…", else: ""
        {prefix <> trimmed <> suffix, start}
      else
        {binary_slice(line, 0, max_width) <> "…", 0}
      end
    end
  end
end
