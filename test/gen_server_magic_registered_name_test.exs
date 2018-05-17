defmodule GenServerMagicRegisteredNamedTest do
  use ExUnit.Case, async: false

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

    defupdate update_pattern_match_state(_id, %{name: :name} = state) do
      Map.put(state, :id, :name)
    end

    defupdate update_pattern_match_state(id, %{age: age} = state) do
      Map.put(state, :id, id)
    end

    defupdate update_pattern_match_state(id, state) do
      state
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
    GenServer.stop(TestModule, :normal)
  end

  test "update with pattern match on state" do
    assert TestModule.Server.update_pattern_match_state(13, %{name: :name}) == %{
             id: :name,
             name: :name
           }

    assert TestModule.Server.update_pattern_match_state(13, %{age: 21}) == %{id: 13, age: 21}

    assert TestModule.Server.update_pattern_match_state(13, "Test name") == "Test name"

    {:ok, _pid} = TestModule.start_link(%{name: "Test name"})
    assert TestModule.update_pattern_match_state(13) == :ok
    GenServer.stop(TestModule, :normal)
    {:ok, _pid} = TestModule.start_link(%{age: 21})
    assert TestModule.update_pattern_match_state(13) == :ok
    GenServer.stop(TestModule, :normal)
    {:ok, _pid} = TestModule.start_link("Test name")
    assert TestModule.update_pattern_match_state(13) == :ok
    GenServer.stop(TestModule, :normal)
  end
end
