defmodule BeamLang.Token do
  @moduledoc """
  Token representation for the BeamLang lexer.
  """

  @type t :: %__MODULE__{
          type: atom(),
          value: any(),
          line: non_neg_integer(),
          col: non_neg_integer(),
          span: BeamLang.Span.t()
        }

  @enforce_keys [:type, :value, :line, :col, :span]
  defstruct [:type, :value, :line, :col, :span]
end
