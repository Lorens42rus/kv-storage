defmodule Kvstore.Storage do
  use GenServer

  @impl true
  def init(_opts) do
    {:ok, nil}
  end

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
end
