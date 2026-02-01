defmodule BeamLang.LSP.Debug do
  @moduledoc false

  @spec enabled?() :: boolean()
  def enabled? do
    case System.get_env("BEAMLANG_LSP_DEBUG") do
      "1" -> true
      "true" -> true
      "TRUE" -> true
      "yes" -> true
      "YES" -> true
      _ -> false
    end
  end

  @spec log(binary() | (() -> binary())) :: :ok
  def log(message) when is_binary(message) do
    if enabled?() do
      IO.binwrite(:stderr, "[beamlang-lsp] " <> message <> "\n")
    end

    :ok
  end

  def log(fun) when is_function(fun, 0) do
    if enabled?() do
      log(fun.())
    else
      :ok
    end
  end
end
