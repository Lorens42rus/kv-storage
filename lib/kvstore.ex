defmodule Kvstore.Application do
  @moduledoc false

  use Application

  @port Application.get_env(:kvstore, :project_port)

  def start(_type, _args) do
    children = [
      {Kvstore.Storage, name: Kvstore.Storage},
      {Plug.Cowboy, scheme: :http, plug: Kvstore.Router, options: [port: @port]},
    ]

    opts = [strategy: :one_for_one, name: Kvstore.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
