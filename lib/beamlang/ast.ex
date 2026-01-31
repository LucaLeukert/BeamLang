defmodule BeamLang.AST do
  @moduledoc """
  AST types for BeamLang. These will expand as the language grows.
  """

  @type type_name ::
          :number
          | :String
          | :char
          | :bool
          | :void
          | :any
          | {:named, binary()}
          | {:generic, type_name(), [type_name()]}
          | {:optional, type_name()}
          | {:result, type_name(), type_name()}
          | {:fn, [type_name()], type_name()}

  @type literal ::
          {:integer, %{value: integer(), span: BeamLang.Span.t()}}
          | {:float, %{value: float(), span: BeamLang.Span.t()}}
          | {:string, %{value: binary(), span: BeamLang.Span.t()}}
          | {:char, %{value: integer(), span: BeamLang.Span.t()}}
          | {:bool, %{value: boolean(), span: BeamLang.Span.t()}}

  @type expr ::
          literal()
          | {:call, %{name: binary(), args: [expr()], span: BeamLang.Span.t()}}
          | {:identifier, %{name: binary(), span: BeamLang.Span.t()}}
          | {:lambda, %{params: [func_param()], return_type: type_name(), body: block(), span: BeamLang.Span.t()}}
          | {:method_call, %{target: expr(), name: binary(), args: [expr()], span: BeamLang.Span.t(), target_type: type_name() | nil}}
          | {:struct, %{fields: [field_assign()], type: type_name() | nil, span: BeamLang.Span.t()}}
          | {:field, %{target: expr(), name: binary(), span: BeamLang.Span.t()}}
          | {:block_expr, %{block: block(), span: BeamLang.Span.t()}}
          | {:match, %{expr: expr(), cases: [match_case()], span: BeamLang.Span.t()}}
          | {:binary, %{op: binary_op(), left: expr(), right: expr(), span: BeamLang.Span.t()}}
          | {:if_expr, %{cond: expr(), then_block: block(), else_branch: if_else_branch(), span: BeamLang.Span.t()}}
          | {:opt_some, %{expr: expr(), span: BeamLang.Span.t(), type: type_name() | nil}}
          | {:opt_none, %{span: BeamLang.Span.t(), type: type_name() | nil}}
          | {:res_ok, %{expr: expr(), span: BeamLang.Span.t(), type: type_name() | nil}}
          | {:res_err, %{expr: expr(), span: BeamLang.Span.t(), type: type_name() | nil}}

  @type binary_op :: :eq | :neq | :lt | :gt | :lte | :gte | :add | :sub | :mul | :div | :mod

  @type pattern ::
          literal()
          | {:wildcard, %{span: BeamLang.Span.t()}}
          | {:pat_identifier, %{name: binary(), span: BeamLang.Span.t()}}
          | {:struct_pattern, %{name: binary(), fields: [pattern_field()], span: BeamLang.Span.t()}}
          | {:opt_some_pat, %{name: binary(), span: BeamLang.Span.t()}}
          | {:opt_none_pat, %{span: BeamLang.Span.t()}}
          | {:res_ok_pat, %{name: binary(), span: BeamLang.Span.t()}}
          | {:res_err_pat, %{name: binary(), span: BeamLang.Span.t()}}

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
             type_params: [binary()],
             params: [func_param()],
             return_type: type_name(),
             body: block() | nil,
             external: map() | nil,
             exported: boolean(),
             internal: boolean(),
             span: BeamLang.Span.t()
           }}

  @type type_def ::
          {:type_def,
           %{
             name: binary(),
             params: [binary()],
             fields: [field_def()],
             exported: boolean(),
             span: BeamLang.Span.t()
           }}

  @type field_def ::
          %{name: binary(), type: type_name(), span: BeamLang.Span.t()}

  @type import_item :: %{name: binary(), span: BeamLang.Span.t()}

  @type import ::
          {:import,
           %{
             module: binary(),
             alias: binary() | nil,
             items: :all | :none | [import_item()],
             span: BeamLang.Span.t()
           }}

  @type program ::
          {:program,
           %{
             module: binary() | nil,
             imports: [import()],
             types: [type_def()],
             functions: [func()],
             span: BeamLang.Span.t()
           }}

  @type t :: program()
end
