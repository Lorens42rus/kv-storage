defmodule Kvstore.MixProject do
  use Mix.Project

  def project do
    [
      app: :kvstore,
      version: "0.1.0",
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Kvstore.Application, []}
    ]
  end

  defp deps do
    [
      {:plug_cowboy, "~> 2.0"},
      {:poison, "~> 3.1"},
    ]
  end
end
