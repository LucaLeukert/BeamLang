defmodule BeamLang.ASTPrinter do
  @moduledoc """
  Pretty-printer for the BeamLang AST.
  """

  @spec format(BeamLang.AST.t()) :: binary()
  def format(ast) do
    ast
    |> format_node(0)
    |> Enum.join("\n")
  end

  defp format_node({:program, %{types: types, functions: functions}}, indent) do
    [
      indent_line(indent, "Program"),
      indent_line(indent + 2, "Types"),
      types
      |> Enum.flat_map(&format_node(&1, indent + 4)),
      indent_line(indent + 2, "Functions"),
      functions
      |> Enum.flat_map(&format_node(&1, indent + 4))
    ]
    |> List.flatten()
  end

  defp format_node({:type_def, %{name: name, fields: fields}}, indent) do
    [
      indent_line(indent, "Type #{name}"),
      indent_line(indent + 2, "Fields"),
      fields
      |> Enum.flat_map(fn %{name: field, type: type} ->
        [indent_line(indent + 4, "#{field}: #{format_type(type)}")]
      end)
    ]
  end

  defp format_node({:function, %{name: name, params: params, return_type: type, body: body}}, indent) do
    params_text =
      params
      |> Enum.map(fn %{name: param_name, type: param_type} ->
        "#{param_name}: #{format_type(param_type)}"
      end)
      |> Enum.join(", ")

    params_line =
      if params_text == "" do
        indent_line(indent + 2, "Params (none)")
      else
        indent_line(indent + 2, "Params #{params_text}")
      end

    [
      indent_line(indent, "Fn #{name} -> #{format_type(type)}"),
      params_line,
      format_node(body, indent + 2)
    ]
  end

  defp format_node({:block, %{stmts: stmts}}, indent) do
    [
      indent_line(indent, "Block"),
      stmts
      |> Enum.flat_map(&format_node(&1, indent + 2))
    ]
  end

  defp format_node({:let, %{name: name, mutable: mutable, type: type, expr: expr}}, indent) do
    mut = if mutable, do: "mut ", else: ""
    type_part = if type == nil, do: "", else: ": #{format_type(type)}"
    [
      indent_line(indent, "Let #{mut}#{name}#{type_part}"),
      format_node(expr, indent + 2)
    ]
  end

  defp format_node({:assign, %{target: target, expr: expr}}, indent) do
    [
      indent_line(indent, "Assign"),
      indent_line(indent + 2, "Target"),
      format_node(target, indent + 4),
      indent_line(indent + 2, "Value"),
      format_node(expr, indent + 4)
    ]
  end

  defp format_node({:return, %{expr: nil}}, indent) do
    [indent_line(indent, "Return (void)")]
  end

  defp format_node({:return, %{expr: expr}}, indent) do
    [
      indent_line(indent, "Return"),
      format_node(expr, indent + 2)
    ]
  end

  defp format_node({:expr, %{expr: expr}}, indent) do
    [
      indent_line(indent, "Expr"),
      format_node(expr, indent + 2)
    ]
  end

  defp format_node({:guard, %{cond: cond, else_block: else_block}}, indent) do
    [
      indent_line(indent, "Guard"),
      indent_line(indent + 2, "Condition"),
      format_node(cond, indent + 4),
      indent_line(indent + 2, "Else"),
      format_node(else_block, indent + 4)
    ]
  end

  defp format_node({:if_stmt, %{cond: cond, then_block: then_block, else_branch: else_branch}}, indent) do
    [
      indent_line(indent, "IfStmt"),
      indent_line(indent + 2, "Condition"),
      format_node(cond, indent + 4),
      indent_line(indent + 2, "Then"),
      format_node(then_block, indent + 4),
      format_else_branch(else_branch, indent + 2)
    ]
    |> List.flatten()
  end

  defp format_node({:while, %{cond: cond, body: body}}, indent) do
    [
      indent_line(indent, "While"),
      indent_line(indent + 2, "Condition"),
      format_node(cond, indent + 4),
      indent_line(indent + 2, "Body"),
      format_node(body, indent + 4)
    ]
  end

  defp format_node({:loop, %{body: body}}, indent) do
    [
      indent_line(indent, "Loop"),
      indent_line(indent + 2, "Body"),
      format_node(body, indent + 4)
    ]
  end

  defp format_node({:for, %{name: name, collection: collection, body: body}}, indent) do
    [
      indent_line(indent, "For #{name}"),
      indent_line(indent + 2, "Collection"),
      format_node(collection, indent + 4),
      indent_line(indent + 2, "Body"),
      format_node(body, indent + 4)
    ]
  end

  defp format_node({:break, %{}}, indent) do
    [indent_line(indent, "Break")]
  end

  defp format_node({:block_expr, %{block: block}}, indent) do
    [
      indent_line(indent, "BlockExpr"),
      format_node(block, indent + 2)
    ]
  end

  defp format_node({:if_expr, %{cond: cond, then_block: then_block, else_branch: else_branch}}, indent) do
    [
      indent_line(indent, "IfExpr"),
      indent_line(indent + 2, "Condition"),
      format_node(cond, indent + 4),
      indent_line(indent + 2, "Then"),
      format_node(then_block, indent + 4),
      format_else_branch(else_branch, indent + 2)
    ]
    |> List.flatten()
  end

  defp format_node({:match, %{expr: expr, cases: cases}}, indent) do
    [
      indent_line(indent, "Match"),
      indent_line(indent + 2, "Value"),
      format_node(expr, indent + 4),
      indent_line(indent + 2, "Cases"),
      cases
      |> Enum.flat_map(fn %{pattern: pattern, guard: guard, body: body} ->
        List.flatten([
          indent_line(indent + 4, "Case"),
          indent_line(indent + 6, "Pattern"),
          format_pattern(pattern, indent + 8),
          guard_section(guard, indent + 6),
          indent_line(indent + 6, "Body"),
          format_node(body, indent + 8)
        ])
      end)
    ]
  end

  defp format_node({:integer, %{value: value}}, indent),
    do: [indent_line(indent, "Int #{value}")]

  defp format_node({:float, %{value: value}}, indent),
    do: [indent_line(indent, "Float #{value}")]

  defp format_node({:string, %{value: value}}, indent),
    do: [indent_line(indent, "String \"#{value}\"")]

  defp format_node({:bool, %{value: value}}, indent),
    do: [indent_line(indent, "Bool #{value}")]

  defp format_node({:identifier, %{name: name}}, indent),
    do: [indent_line(indent, "Id #{name}")]

  defp format_node({:call, %{name: name, args: args}}, indent) do
    [
      indent_line(indent, "Call #{name}"),
      args |> Enum.flat_map(&format_node(&1, indent + 2))
    ]
  end

  defp format_node({:struct, %{fields: fields}}, indent) do
    [
      indent_line(indent, "Struct"),
      fields
      |> Enum.flat_map(fn %{name: name, expr: expr} ->
        [
          indent_line(indent + 2, "Field #{name}"),
          format_node(expr, indent + 4)
        ]
      end)
    ]
  end

  defp format_node({:field, %{target: target, name: name}}, indent) do
    [
      indent_line(indent, "FieldAccess #{name}"),
      format_node(target, indent + 2)
    ]
  end

  defp format_node({:binary, %{op: op, left: left, right: right}}, indent) do
    [
      indent_line(indent, "Binary #{op}"),
      format_node(left, indent + 2),
      format_node(right, indent + 2)
    ]
  end

  defp format_pattern({:integer, %{value: value}}, indent),
    do: [indent_line(indent, "Int #{value}")]

  defp format_pattern({:float, %{value: value}}, indent),
    do: [indent_line(indent, "Float #{value}")]

  defp format_pattern({:string, %{value: value}}, indent),
    do: [indent_line(indent, "String \"#{value}\"")]

  defp format_pattern({:bool, %{value: value}}, indent),
    do: [indent_line(indent, "Bool #{value}")]

  defp format_pattern({:wildcard, %{}}, indent),
    do: [indent_line(indent, "Wildcard")]

  defp format_pattern({:pat_identifier, %{name: name}}, indent),
    do: [indent_line(indent, "Bind #{name}")]

  defp format_pattern({:struct_pattern, %{name: name, fields: fields}}, indent) do
    [
      indent_line(indent, "StructPattern #{name}"),
      fields
      |> Enum.flat_map(fn %{name: field, pattern: pattern} ->
        [
          indent_line(indent + 2, "Field #{field}"),
          format_pattern(pattern, indent + 4)
        ]
      end)
    ]
  end

  defp format_else_branch(nil, _indent), do: []

  defp format_else_branch({:else_block, %{block: block}}, indent) do
    [
      indent_line(indent, "Else"),
      format_node(block, indent + 2)
    ]
  end

  defp format_else_branch({:else_if, %{if: if_stmt}}, indent) do
    [
      indent_line(indent, "ElseIf"),
      format_node(if_stmt, indent + 2)
    ]
  end

  defp guard_section(nil, _indent), do: []

  defp guard_section(guard, indent) do
    [
      indent_line(indent, "Guard"),
      format_node(guard, indent + 2)
    ]
  end

  defp format_type({:named, name}), do: name
  defp format_type(type) when is_atom(type), do: Atom.to_string(type)

  defp indent_line(indent, text) do
    String.duplicate(" ", indent) <> text
  end
end
