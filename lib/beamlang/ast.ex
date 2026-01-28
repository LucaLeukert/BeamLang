defmodule BeamLang.AST do
  @moduledoc """
  AST types for BeamLang. These will expand as the language grows.
  """

  @type type_name :: :i32 | :i64 | :f32 | :f64 | :String | :bool | :void | {:named, binary()}

  @type literal ::
          {:integer, %{value: integer(), span: BeamLang.Span.t()}}
          | {:float, %{value: float(), span: BeamLang.Span.t()}}
          | {:string, %{value: binary(), span: BeamLang.Span.t()}}
          | {:bool, %{value: boolean(), span: BeamLang.Span.t()}}

  @type expr ::
          literal()
          | {:call, %{name: binary(), args: [expr()], span: BeamLang.Span.t()}}
          | {:identifier, %{name: binary(), span: BeamLang.Span.t()}}
          | {:struct, %{fields: [field_assign()], span: BeamLang.Span.t()}}
          | {:field, %{target: expr(), name: binary(), span: BeamLang.Span.t()}}
          | {:block_expr, %{block: block(), span: BeamLang.Span.t()}}
          | {:match, %{expr: expr(), cases: [match_case()], span: BeamLang.Span.t()}}
          | {:binary, %{op: binary_op(), left: expr(), right: expr(), span: BeamLang.Span.t()}}
          | {:if_expr, %{cond: expr(), then_block: block(), else_branch: if_else_branch(), span: BeamLang.Span.t()}}

  @type binary_op :: :eq | :neq | :lt | :gt | :lte | :gte

  @type pattern ::
          literal()
          | {:wildcard, %{span: BeamLang.Span.t()}}
          | {:pat_identifier, %{name: binary(), span: BeamLang.Span.t()}}
          | {:struct_pattern, %{name: binary(), fields: [pattern_field()], span: BeamLang.Span.t()}}

  @type pattern_field ::
          %{name: binary(), pattern: pattern(), span: BeamLang.Span.t()}

  @type match_case ::
          %{pattern: pattern(), guard: expr() | nil, body: expr(), span: BeamLang.Span.t()}

  @type field_assign ::
          %{name: binary(), expr: expr(), span: BeamLang.Span.t()}

  @type stmt ::
          {:return, %{expr: expr() | nil, span: BeamLang.Span.t()}}
          | {:expr, %{expr: expr(), span: BeamLang.Span.t()}}
          | {:let,
             %{
               name: binary(),
               mutable: boolean(),
               type: type_name() | nil,
               expr: expr(),
               span: BeamLang.Span.t()
             }}
          | {:assign, %{target: expr(), expr: expr(), span: BeamLang.Span.t()}}
          | {:guard, %{cond: expr(), else_block: block(), span: BeamLang.Span.t()}}
          | {:if_stmt, %{cond: expr(), then_block: block(), else_branch: if_else_branch() | nil, span: BeamLang.Span.t()}}
          | {:while, %{cond: expr(), body: block(), span: BeamLang.Span.t()}}
          | {:loop, %{body: block(), span: BeamLang.Span.t()}}
          | {:for, %{name: binary(), collection: expr(), body: block(), span: BeamLang.Span.t()}}
          | {:break, %{span: BeamLang.Span.t()}}

  @type if_else_branch :: {:else_block, %{block: block(), span: BeamLang.Span.t()}} | {:else_if, %{if: stmt(), span: BeamLang.Span.t()}}

  @type block ::
          {:block, %{stmts: [stmt()], span: BeamLang.Span.t()}}

  @type func_param ::
          %{name: binary(), type: type_name(), span: BeamLang.Span.t()}

  @type func ::
          {:function,
           %{
             name: binary(),
             params: [func_param()],
             return_type: type_name(),
             body: block(),
             span: BeamLang.Span.t()
           }}

  @type type_def ::
          {:type_def,
           %{
             name: binary(),
             fields: [field_def()],
             span: BeamLang.Span.t()
           }}

  @type field_def ::
          %{name: binary(), type: type_name(), span: BeamLang.Span.t()}

  @type program ::
          {:program, %{types: [type_def()], functions: [func()], span: BeamLang.Span.t()}}

  @type t :: program()
end
