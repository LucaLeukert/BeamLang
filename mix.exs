defmodule Beamlang.MixProject do
  use Mix.Project

  def project do
    [
      app: :beamlang,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      escript: [main_module: BeamLang.CLI, name: "beamlang"],
      deps: [
        {:jason, "~> 1.4"},
        {:elixir_lsp, "~> 0.2.1"}
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger, :inets, :ssl]
    ]
  end
end
