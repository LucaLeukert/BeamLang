defmodule BeamLang.Error do
  @moduledoc """
  Structured compiler error with span information.
  """

  @type kind :: :lexer | :parser | :type

  @type t :: %__MODULE__{
          kind: kind(),
          message: binary(),
          span: BeamLang.Span.t(),
          notes: [binary()]
        }

  defstruct [:kind, :message, :span, notes: []]

  @spec new(kind(), binary(), BeamLang.Span.t()) :: t()
  def new(kind, message, span) do
    %__MODULE__{kind: kind, message: message, span: span}
  end

  @spec with_notes(t(), [binary()]) :: t()
  def with_notes(error, notes) do
    %{error | notes: notes}
  end
end
