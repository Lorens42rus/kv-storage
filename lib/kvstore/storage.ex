defmodule Kvstore.Storage do
  use GenServer

  @storage_name Application.get_env(:kvstore, :storage_table_name)

  @impl true
  def init(_opts) do
    case :dets.open_file(:storage, [{:file, @storage_name}]) do
      {:ok, table} ->
        spawn_link(__MODULE__, :handle_ttl, [table])
        {:ok, table}

      {:error, msg} ->
        raise msg
    end
  end

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @spec create(String.t(), map | list, non_neg_integer) :: {:ok, nil} | {:error, String.t()}
  def create(key, val, ttl \\ 0) do
    GenServer.call(__MODULE__, {:create, key, val, ttl})
  end

  @spec read(String.t()) :: {:ok, map | list} | {:error, String.t()}
  def read(key) do
    GenServer.call(__MODULE__, {:read, key})
  end

  @spec update(String.t(), map | list, non_neg_integer) :: :ok
  def update(key, val, ttl \\ 0) do
    GenServer.call(__MODULE__, {:update, key, val, ttl})
  end

  @spec delete(String.t()) :: :ok
  def delete(key) do
    GenServer.cast(__MODULE__, {:delete, key})
  end

  @spec handle_ttl(any) :: no_return
  def handle_ttl(table) do
    now = DateTime.utc_now() |> DateTime.to_unix()

    :dets.select_delete(table, [
      {{:"$1", :"$2", :_}, [{:andalso, {:>, :"$2", 0}, {:"=<", :"$2", now}}], [true]}
    ])

    Process.sleep(1000)
    handle_ttl(table)
  end

  @spec get_expired_at(non_neg_integer) :: number
  defp get_expired_at(ttl) do
    if ttl > 0 do
      (DateTime.utc_now()
       |> DateTime.to_unix()) + ttl
    else
      0
    end
  end

  @impl true
  def handle_call({:create, key, val, ttl}, _, table) do
    case :dets.insert_new(table, {key, get_expired_at(ttl), val}) do
      true ->
        {:reply, {:ok, nil}, table}

      false ->
        {:reply, {:error, "Value by key '#{key}' already exists"}, table}
    end
  end

  @impl true
  def handle_call({:read, key}, _, table) do
    case :dets.lookup(table, key) do
      [{_, _, item}] -> {:reply, {:ok, item}, table}
      _ -> {:reply, {:error, "Item by key '#{key}' not found"}, table}
    end
  end

  @impl true
  def handle_call({:update, key, val, ttl}, _, table) do
    :dets.insert(table, {key, get_expired_at(ttl), val})
    {:reply, :ok, table}
  end

  @impl true
  def handle_cast({:delete, key}, table) do
    :dets.delete(table, key)
    {:noreply, table}
  end

  @impl true
  def terminate(_reason, table) do
    :dets.close(table)
    table
  end
end
