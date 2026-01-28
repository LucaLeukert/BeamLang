defmodule BeamLang.Span do
  @moduledoc """
  Source span information for AST nodes and tokens.
  """

  @type t :: %__MODULE__{
          file_id: binary(),
          start: non_neg_integer(),
          end: non_neg_integer()
        }

  defstruct [:file_id, :start, :end]

  @spec new(binary(), non_neg_integer(), non_neg_integer()) :: t()
  def new(file_id, start, end_pos) when is_binary(file_id) do
    %__MODULE__{file_id: file_id, start: start, end: end_pos}
  end

  @spec first(t(), t()) :: t()
  def first(a, b) do
    if a.start <= b.start, do: a, else: b
  end

  @spec last(t(), t()) :: t()
  def last(a, b) do
    if a.end >= b.end, do: a, else: b
  end

  @spec merge(t(), t()) :: t()
  def merge(a, b) do
    start = min(a.start, b.start)
    end_pos = max(a.end, b.end)
    %__MODULE__{file_id: a.file_id, start: start, end: end_pos}
  end

  @spec contains(t(), t()) :: boolean()
  def contains(span, inner) do
    span.file_id == inner.file_id and span.start <= inner.start and span.end >= inner.end
  end

  @spec is_in_offset_range(t(), non_neg_integer(), non_neg_integer()) :: boolean()
  def is_in_offset_range(span, start, end_pos) do
    span.start >= start and span.end <= end_pos
  end

  @spec debug_description(t()) :: binary()
  def debug_description(span) do
    "#{span.file_id}:#{span.start}-#{span.end}"
  end
end
