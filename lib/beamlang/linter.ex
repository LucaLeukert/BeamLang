defmodule BeamLang.Linter do
  @moduledoc """
  Token-level linting and autofix rules for BeamLang source.
  """

  alias BeamLang.{Lexer, Token}

  @type diagnostic :: %{
          rule: atom(),
          message: binary(),
          line: non_neg_integer(),
          col: non_neg_integer(),
          span: BeamLang.Span.t()
        }

  @spec lint(binary(), binary()) :: {:ok, [diagnostic()]} | {:error, binary()}
  def lint(source, path \\ "<lint>") when is_binary(source) and is_binary(path) do
    case Lexer.tokenize(source, path) do
      {:ok, tokens} ->
        %{diagnostics: diagnostics} = lint_tokens(tokens)
        {:ok, diagnostics}

      {:error, error} ->
        {:error, inspect(error)}
    end
  end

  @spec lint_tokens([Token.t()]) :: %{tokens: [Token.t()], diagnostics: [diagnostic()]}
  def lint_tokens(tokens) when is_list(tokens) do
    {rewritten, diagnostics} = rewrite_single_expression_branches(tokens)
    %{tokens: rewritten, diagnostics: diagnostics}
  end

  @spec autocorrect_tokens([Token.t()]) :: [Token.t()]
  def autocorrect_tokens(tokens) when is_list(tokens) do
    %{tokens: rewritten} = lint_tokens(tokens)
    rewritten
  end

  defp rewrite_single_expression_branches(tokens) do
    do_rewrite_single_expression_branches(tokens)
  end

  defp do_rewrite_single_expression_branches([]), do: {[], []}

  defp do_rewrite_single_expression_branches([
         %Token{type: :arrow} = arrow,
         %Token{type: :lbrace} = lbrace | rest
       ]) do
    case take_brace_contents(rest, 1, []) do
      {:ok, inner, closing, remaining} ->
        {rewritten_inner, inner_diagnostics} = rewrite_single_expression_branches(inner)

        if single_expression_block?(rewritten_inner) and branch_separator?(remaining) do
          expr_tokens = strip_trailing_top_level_semicolon(rewritten_inner)
          fat_arrow = %{arrow | type: :fat_arrow, value: "=>"}
          diagnostic = branch_rewrite_diagnostic(arrow)
          {rewritten_rest, rest_diagnostics} = do_rewrite_single_expression_branches(remaining)

          {[fat_arrow | expr_tokens] ++ rewritten_rest,
           inner_diagnostics ++ [diagnostic] ++ rest_diagnostics}
        else
          {rewritten_rest, rest_diagnostics} = do_rewrite_single_expression_branches(remaining)

          {[arrow, lbrace] ++ rewritten_inner ++ [closing] ++ rewritten_rest,
           inner_diagnostics ++ rest_diagnostics}
        end

      :error ->
        {rewritten_rest, rest_diagnostics} =
          do_rewrite_single_expression_branches([lbrace | rest])

        {[arrow | rewritten_rest], rest_diagnostics}
    end
  end

  defp do_rewrite_single_expression_branches([tok | rest]) do
    {rewritten_rest, diagnostics} = do_rewrite_single_expression_branches(rest)
    {[tok | rewritten_rest], diagnostics}
  end

  defp branch_rewrite_diagnostic(%Token{} = tok) do
    %{
      rule: :single_expression_match_branch,
      message: "Use '=>' for single-expression match branches.",
      line: tok.line,
      col: tok.col,
      span: tok.span
    }
  end

  defp take_brace_contents([], _depth, _inner), do: :error

  defp take_brace_contents([%Token{type: :lbrace} = tok | rest], depth, inner) do
    take_brace_contents(rest, depth + 1, [tok | inner])
  end

  defp take_brace_contents([%Token{type: :rbrace} = tok | rest], 1, inner) do
    {:ok, Enum.reverse(inner), tok, rest}
  end

  defp take_brace_contents([%Token{type: :rbrace} = tok | rest], depth, inner) do
    take_brace_contents(rest, depth - 1, [tok | inner])
  end

  defp take_brace_contents([tok | rest], depth, inner) do
    take_brace_contents(rest, depth, [tok | inner])
  end

  defp single_expression_block?(tokens) do
    top_level_semicolons =
      tokens
      |> top_level_token_indices(:semicolon)

    case top_level_semicolons do
      [semicolon_idx] ->
        semicolon_idx == length(tokens) - 1 and
          top_level_statement_keyword_count(tokens) == 0

      _ ->
        false
    end
  end

  defp strip_trailing_top_level_semicolon(tokens) do
    case Enum.reverse(tokens) do
      [%Token{type: :semicolon} | rest] -> Enum.reverse(rest)
      _ -> tokens
    end
  end

  defp top_level_token_indices(tokens, type) do
    {_stack, _idx, indices} =
      Enum.reduce(tokens, {[], 0, []}, fn tok, {stack, idx, indices} ->
        new_stack =
          case tok.type do
            t when t in [:lparen, :lbracket, :lbrace] -> [tok.type | stack]
            :rparen -> drop_matching(stack, :lparen)
            :rbracket -> drop_matching(stack, :lbracket)
            :rbrace -> drop_matching(stack, :lbrace)
            _ -> stack
          end

        new_indices =
          if stack == [] and tok.type == type do
            [idx | indices]
          else
            indices
          end

        {new_stack, idx + 1, new_indices}
      end)

    Enum.reverse(indices)
  end

  defp top_level_statement_keyword_count(tokens) do
    statement_keywords = [
      :let,
      :let_kw,
      :return,
      :return_kw,
      :if,
      :if_kw,
      :while,
      :while_kw,
      :for,
      :for_kw,
      :loop,
      :loop_kw,
      :guard,
      :guard_kw
    ]

    {_stack, count} =
      Enum.reduce(tokens, {[], 0}, fn tok, {stack, count} ->
        new_stack =
          case tok.type do
            t when t in [:lparen, :lbracket, :lbrace] -> [tok.type | stack]
            :rparen -> drop_matching(stack, :lparen)
            :rbracket -> drop_matching(stack, :lbracket)
            :rbrace -> drop_matching(stack, :lbrace)
            _ -> stack
          end

        new_count =
          if stack == [] and tok.type in statement_keywords do
            count + 1
          else
            count
          end

        {new_stack, new_count}
      end)

    count
  end

  defp drop_matching([expected | rest], expected), do: rest
  defp drop_matching(stack, _expected), do: stack

  defp branch_separator?([%Token{type: type} | _]) when type in [:comma, :rbrace], do: true
  defp branch_separator?([]), do: true
  defp branch_separator?(_), do: false
end
