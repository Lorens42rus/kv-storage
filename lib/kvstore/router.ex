defmodule Kvstore.Router do
  @moduledoc """
  Роутер, предоставляющий WEB API для работы с модулем Storage
  """

  use Plug.Router
  use Plug.ErrorHandler
  require Logger

  plug(
    Plug.Parsers,
    parsers: [:urlencoded, :json],
    pass: ["applicaton/json"],
    json_decoder: Poison
  )

  plug(:match)
  plug(:dispatch)

  @spec send_data(Plug.Conn.t(), non_neg_integer, map()) :: Plug.Conn.t()
  defp send_data(conn, status, data) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, Poison.encode!(data))
  end

  get "/" do
    with %{"key" => key} <- conn.query_params,
         {:ok, item} <- Kvstore.Storage.read(key) do
      send_data(conn, 200, %{"value" => item})
    else
      {:error, msg} ->
        send_data(conn, 400, %{"errors" => [msg]})

      _ ->
        send_data(conn, 200, %{})
    end
  end

  put "/" do
    case conn.query_params do
      %{"key" => key, "ttl" => ttl} ->
        with {val, _} <- Integer.parse(ttl),
             {:ok, _} <- Kvstore.Storage.create(key, conn.body_params, val) do
          send_data(conn, 200, %{})
        else
          {:error, msg} ->
            send_data(conn, 400, %{"errors" => [msg]})
          :error ->
            send_data(conn, 400, %{"errors" => ["TTL should be a number"]})
        end
      %{"key" => key} ->
        with {:ok, _} <- Kvstore.Storage.create(key, conn.body_params) do
          send_data(conn, 200, %{})
        else
          {:error, msg} ->
            send_data(conn, 400, %{"errors" => [msg]})
        end
      _ ->
        send_data(conn, 400, %{"errors" => ["Param 'key' required"]})
    end
  end

  post "/" do
    case conn.query_params do
      %{"key" => key, "ttl" => ttl} ->
        with {val, _} <- Integer.parse(ttl) do
          Kvstore.Storage.update(key, conn.body_params, val)
          send_data(conn, 200, %{})
        else
          :error ->
            send_data(conn, 400, %{"errors" => ["TTL should be a number"]})
        end
      %{"key" => key} ->
        Kvstore.Storage.update(key, conn.body_params)
        send_data(conn, 200, %{})
      _ ->
        send_data(conn, 400, %{"errors" => ["Param 'key' required"]})
    end
  end

  delete "/" do
    case conn.query_params do
      %{"key" => key} ->
        Kvstore.Storage.delete(key)
        send_data(conn, 200, %{})

      _ ->
        send_data(conn, 400, %{"errors" => ["Param 'key' required"]})
    end
  end

  match _ do
    send_data(conn, 404, %{})
  end

  def handle_errors(conn, %{kind: _kind, reason: reason, stack: _stack}) do
    case reason do
      %Plug.Parsers.ParseError{} ->
        send_data(
          conn,
          reason.plug_status,
          %{"errors" => ["Invalid JSON format"]}
        )
      _ ->
        send_data(
          conn,
          500,
          %{"errors" => ["Something went wrong"]}
        )
    end
  end
end
