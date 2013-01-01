defmodule MsgPack.Mixfile do
  use Mix.Project

  def project do
    [ app: :exmsgpack,
      version: "0.0.1",
      deps: deps ]
  end

  def application do
    []
  end

  defp deps do
    [] ++ deps(Mix.env)
  end

  defp deps(:test) do
    [
     {:properex, github: "yrashk/properex"},
    ]
  end
  defp deps(_), do: []
end
