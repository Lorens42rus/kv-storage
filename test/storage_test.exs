defmodule StorageTest do
  use ExUnit.Case, async: true

  @table_name Application.get_env(:kvstore, :storage_table_name)

  setup_all do
    on_exit(fn -> File.rm(@table_name) end)
    :ok
  end

  test "create" do
    err = {:error, "Value by key 'test1' already exists"}

    assert Kvstore.Storage.create("test1", %{"key" => "val"}, 1) == {:ok, nil}
    assert Kvstore.Storage.create("test1", %{"key" => "val"}) == err

    Process.sleep(2000)
    assert Kvstore.Storage.create("test1", %{"key" => "val"}) == {:ok, nil}
  end

  test "read" do
    err = {:error, "Item by key 'test22' not found"}

    assert Kvstore.Storage.create("test2", %{"key" => "val"}) == {:ok, nil}
    assert Kvstore.Storage.read("test2") == {:ok, %{"key" => "val"}}
    assert Kvstore.Storage.read("test22") == err
  end

  test "update" do
    data = %{"key" => "val2"}

    assert Kvstore.Storage.create("test3", %{"key" => "val"}) == {:ok, nil}
    assert Kvstore.Storage.update("test3", data) == :ok
    assert Kvstore.Storage.read("test3") == {:ok, data}
  end

  test "delete" do
    err = {:error, "Item by key 'test4' not found"}

    assert Kvstore.Storage.create("test4", %{"key" => "val"}) == {:ok, nil}
    Kvstore.Storage.delete("test4")
    assert Kvstore.Storage.read("test4") == err
  end
end
