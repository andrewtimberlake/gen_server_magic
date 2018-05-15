defmodule GenServerMagicTest do
  use ExUnit.Case

  defmodule Model do
    defstruct name: nil
  end

  defmodule TestModule do
    use GenServerMagic

    defserver do
      alias GenServerMagicTest.Model
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

    defcall call_with_from_and_state(a, b, {from, state}) do
      {:reply, {a, b, {from, state}}, state}
    end

    defcall call_with_state(a, b, state) do
      {:reply, {a, b, state}, state}
    end

    defcall(call_with_optional_args(a, b, c \\ :optional, state))

    defcall call_with_optional_args(a, b, c, state) do
      {:reply, {a, b, c, state}, state}
    end

    defcall early_return({from, {test, state}}) do
      GenServer.reply(from, :ok)
      Process.sleep(100)
      send(test, :done)
      {:noreply, state + 1}
    end

    defcall match_on_struct(%Model{name: name}, state) do
      {:reply, name, state}
    end

    defcast cast_with_state(a, list) do
      [a | list]
    end

    definfo({:info_with_state, a, b}, list) do
      [b | [a | list]]
    end

    definfo(:some_message, state) do
      state
    end

    defterminate(:shutdown, _state) do
      send(self(), :shutdown)
      :ok
    end

    defterminate(reason, _state) do
      send(self(), {:other, reason})
      :ok
    end
  end

  describe "child_spec/1" do
    test "it produces an initial child_spec" do
      assert TestModule.child_spec(:opts) == %{
               id: TestModule.Server,
               start: {TestModule, :start_link, [:opts]},
               type: :worker,
               restart: :permanent,
               shutdown: 500
             }
    end
  end

  test "handle_call early reply on implementation" do
    assert TestModule.Server.early_return({{self(), :tag}, {self(), 1}}) == {:noreply, 2}
    assert_received :done
    assert_received {:tag, :ok}
  end

  test "handle_call early reply via GenServer" do
    {:ok, pid} = TestModule.start_link({self(), 1})
    assert :ok = TestModule.early_return(pid)
    # The message hasn't been sent before the test reply
    refute_receive :done, 0
    # The message has been sent
    assert_receive :done, 200
    assert TestModule.get_state(pid) == 2
  end

  test "call with struct match" do
    assert TestModule.Server.match_on_struct(%Model{name: "Test name"}, nil) ==
             {:reply, "Test name", nil}

    {:ok, pid} = TestModule.start_link(nil)
    assert TestModule.match_on_struct(pid, %Model{name: "Test name"}) == "Test name"
  end

  describe "definit/1" do
    test "it generates an init callback" do
      assert {:ok, :state} = TestModule.Server.init(:state)
    end
  end

  describe "defcall/2" do
    test "it generates a handle_call callback with state" do
      assert {:reply, {1, 2, :state}, :state} = TestModule.Server.call_with_state(1, 2, :state)

      {:ok, pid} = TestModule.start_link(3)
      assert {1, 2, 3} = TestModule.call_with_state(pid, 1, 2)
    end

    test "it generates a handle_call callback with from and state" do
      assert {:reply, {1, 2, {:from, :state}}, :state} =
               TestModule.Server.call_with_from_and_state(1, 2, {:from, :state})

      {:ok, pid} = TestModule.start_link(3)
      assert {1, 2, {from, 3}} = TestModule.call_with_from_and_state(pid, 1, 2)
      {pid, reference} = from
      assert is_pid(pid)
      assert is_reference(reference)
    end

    test "it allows optional arguments in API interface" do
      assert {:reply, {1, 2, 3, :state}, :state} =
               TestModule.Server.call_with_optional_args(1, 2, 3, :state)

      {:ok, pid} = TestModule.start_link(3)
      assert {1, 2, :optional, 3} = TestModule.call_with_optional_args(pid, 1, 2)
    end
  end

  describe "defcast/2" do
    test "it generates a handle_cast callback" do
      assert [2, 1] = TestModule.Server.cast_with_state(2, [1])

      {:ok, pid} = TestModule.start_link([1])
      assert TestModule.cast_with_state(pid, 2)
      assert TestModule.get_state(pid) == [2, 1]
    end
  end

  describe "defget/2" do
    test "return the state" do
      assert {:state, :state} = TestModule.Server.get_state(:state)

      {:ok, pid} = TestModule.start_link(:state)
      assert TestModule.get_state(pid) == :state
    end

    test "update and return a value" do
      assert {2, [1]} = TestModule.Server.pop([1, 2])

      {:ok, pid} = TestModule.start_link([1, 2, 3])
      assert TestModule.pop(pid) == 3
      assert TestModule.get_state(pid) == [1, 2]
    end
  end

  describe "defupdate/2" do
    test "updates the state" do
      assert [1, 2] = TestModule.Server.push(2, [1])

      {:ok, pid} = TestModule.start_link([1])
      TestModule.push(pid, 2)
      assert TestModule.get_state(pid) == [1, 2]
    end

    test "update and return a value" do
      assert {2, [1]} = TestModule.Server.pop([1, 2])

      {:ok, pid} = TestModule.start_link([1, 2, 3])
      assert TestModule.pop(pid) == 3
      assert TestModule.get_state(pid) == [1, 2]
    end
  end

  describe "definfo/2" do
    test "it generates a handle_info callback" do
      assert [3, 2, 1] = TestModule.Server.info({:info_with_state, 2, 3}, [1])

      {:ok, pid} = TestModule.start_link([1])
      send(pid, {:info_with_state, 2, 3})
      assert TestModule.get_state(pid) == [3, 2, 1]
    end

    test "it generates a handle_info callback that handles arbitrary arguments" do
      assert :state = TestModule.Server.info(:some_message, :state)

      {:ok, pid} = TestModule.start_link(:state)
      send(pid, :some_message)
      assert TestModule.get_state(pid) == :state
    end

    @tag :capture_log
    test "unhandled message" do
      {:ok, pid} = TestModule.start_link(:state)
      Process.flag(:trap_exit, true)
      send(pid, :wrong_message)

      assert_receive {:EXIT, ^pid, _}
    end
  end

  describe "defterminate/2" do
    test "it generates a terminate callback" do
      assert :ok = TestModule.Server.terminate(:shutdown, :state)

      {:ok, pid} = TestModule.start_link(:state)
      assert :ok = GenServer.stop(pid, :normal)
      assert_receive :shutdown
    end

    test "it generates multiple terminate callback" do
      assert :ok = TestModule.Server.terminate(:reason, :state)

      {:ok, pid} = TestModule.start_link(:state)
      Process.exit(pid, :normal)
      assert_receive {:other, :reason}
    end
  end
end
