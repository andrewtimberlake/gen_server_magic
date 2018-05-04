defmodule GenServerMagicRegisteredNamedTest do
  use ExUnit.Case

  defmodule TestModule do
    use GenServerMagic, registered_name: __MODULE__

    def child_spec(_opts) do
      %{
        id: __MODULE__,
        start: {__MODULE__, :start_link, [:fixed_opts]},
        shutdown: 5000
      }
    end

    definit(args) do
      {:ok, args}
    end

    defget get_state(state) do
      {state, state}
    end

    defget pop(list) do
      [item | tail] = Enum.reverse(list)
      {item, Enum.reverse(tail)}
    end

    defupdate push(item, list) do
      Enum.reverse([item | list])
    end
  end

  describe "child_spec/1" do
    test "child_spec can be overridden" do
      assert TestModule.child_spec(:opts) == %{
               id: TestModule,
               start: {TestModule, :start_link, [:fixed_opts]},
               shutdown: 5000
             }
    end
  end

  test "it starts the server with a registered name" do
    {:ok, pid} = TestModule.start_link([1])
    assert ^pid = GenServer.whereis(TestModule)
    TestModule.push(2)
    assert 2 == TestModule.pop()
  end
end
