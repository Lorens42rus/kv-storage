defmodule PlugTest do
  use ExUnit.Case, async: true
  use Plug.Test
  require Logger

  @table_name Application.get_env(:kvstore, :storage_table_name)

  setup_all do
    on_exit(fn -> File.rm(@table_name) end)
    :ok
  end

  test "get" do
    samples = [
      {"", %{}, 200},
      {"?key=test5", %{"value" => %{"key" => "val"}}, 200},
      {"?key=test55", %{"errors" => ["Item by key 'test55' not found"]}, 400}
    ]

    assert Kvstore.Storage.create("test5", %{"key" => "val"}) == {:ok, nil}

    Enum.each(
      samples,
      fn {query, result, status} ->
        resp = conn(:get, "/#{query}")
          |> put_req_header("content-type", "application/json")
          |> Kvstore.Router.call([])
        assert Poison.decode!(resp.resp_body) == result
        assert resp.status == status
      end
    )
  end

  test "put" do
    samples = [
      {"", %{}, %{"errors" => ["Param 'key' required"]}, 400},
      {"?key=test6", %{"key" => "val"}, %{}, 200},
      {"?key=test66&ttl=a", %{}, %{"errors" => ["TTL should be a number"]}, 400},
      {"?key=test66&ttl=1", %{"key" => "val"}, %{}, 200},
      {"?key=test66&ttl=1", %{}, %{"errors" => ["Value by key 'test66' already exists"]}, 400},
      {"?key=test6", %{}, %{"errors" => ["Value by key 'test6' already exists"]}, 400}
    ]

    Enum.each(
      samples,
      fn {query, data, result, status} ->
        resp = conn(:put, "/#{query}", Poison.encode!(data))
          |> put_req_header("content-type", "application/json")
          |> Kvstore.Router.call([])

        assert Poison.decode!(resp.resp_body) == result
        assert resp.status == status
      end
    )

    assert Kvstore.Storage.read("test6") == {:ok, %{"key" => "val"}}
    assert Kvstore.Storage.read("test66") == {:ok, %{"key" => "val"}}
    Process.sleep(2000)
    assert Kvstore.Storage.read("test66") == {:error, "Item by key 'test66' not found"}
  end

  test "post" do
    samples = [
      {"", %{}, %{"errors" => ["Param 'key' required"]}, 400},
      {"?key=test7", %{"key" => "val2"}, %{}, 200},
      {"?key=test77&ttl=a", %{}, %{"errors" => ["TTL should be a number"]}, 400},
      {"?key=test77&ttl=1", %{"key" => "val2"}, %{}, 200}
    ]

    assert Kvstore.Storage.create("test7", %{"key" => "val"}) == {:ok, nil}
    assert Kvstore.Storage.read("test7") == {:ok, %{"key" => "val"}}

    Enum.each(
      samples,
      fn {query, data, result, status} ->
        resp = conn(:post, "/#{query}", Poison.encode!(data))
          |> put_req_header("content-type", "application/json")
          |> Kvstore.Router.call([])

        assert Poison.decode!(resp.resp_body) == result
        assert resp.status == status
      end
    )

    assert Kvstore.Storage.read("test7") == {:ok, %{"key" => "val2"}}
    assert Kvstore.Storage.read("test77") == {:ok, %{"key" => "val2"}}
    Process.sleep(2000)
    assert Kvstore.Storage.read("test77") == {:error, "Item by key 'test77' not found"}
  end

  test "delete" do
    samples = [
      {"", %{"errors" => ["Param 'key' required"]}, 400},
      {"?key=test8", %{}, 200},
    ]

    assert Kvstore.Storage.create("test8", %{}) == {:ok, nil}
    assert Kvstore.Storage.read("test8") == {:ok, %{}}

    Enum.each(
      samples,
      fn {query, result, status} ->
        resp = conn(:delete, "/#{query}")
          |> put_req_header("content-type", "application/json")
          |> Kvstore.Router.call([])

        assert Poison.decode!(resp.resp_body) == result
        assert resp.status == status
      end
    )

    assert Kvstore.Storage.read("test8") == {:error, "Item by key 'test8' not found"}
  end

  test "404 page" do
    resp = conn(:get, "/any")
      |> Kvstore.Router.call([])

    assert resp.status == 404
  end
end
