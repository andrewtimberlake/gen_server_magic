defmodule GenServerMagic.MixProject do
  use Mix.Project

  @version "0.0.1"
  @github_url "https://github.com/andrewtimberlake/gen_server_magic"

  def project do
    [
      app: :gen_server_magic,
      name: "GenServerMagic",
      description: """
      Unashamedly magic, opinionated GenServer macros for greater sanity
      """,
      license: "MIT",
      version: @version,
      elixir: "~> 1.3",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      source_url: @github_url,
      docs: fn ->
        [
          source_ref: "v#{@version}",
          canonical: "https://hexdocs.pm/gen_server_magic",
          main: "GenServerMagic",
          source_url: @github_url,
          extras: ["README.md"]
        ]
      end,
      package: [
        maintainers: ["Andrew Timberlake"],
        contributors: ["Andrew Timberlake"],
        licenses: ["MIT"],
        links: %{"GitHub" => @github_url}
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [{:ex_doc, "~> 0.0", only: [:docs, :dev]}]
  end
end
