defmodule GenServerMagicNamedTest do
  use ExUnit.Case

  defmodule TestModule do
    use GenServerMagic, server_module: TestModuleServer

    definit(arg) do
      {:ok, arg}
    end

    defget pop(list) do
      [item | tail] = Enum.reverse(list)
      {item, Enum.reverse(tail)}
    end

    defupdate push(item, list) do
      Enum.reverse([item | list])
    end
  end

  test "it generates the server module with the specified name" do
    assert [1, 2] = TestModuleServer.push(2, [1])
    assert {2, [1]} = TestModuleServer.pop([1, 2])
  end
end
