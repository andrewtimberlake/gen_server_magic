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
      state
    end

    defget get_with_timeout(state, timeout: 500) do
      Process.sleep(1000)
      state
    end

    defget pop(list) do
      [item | tail] = Enum.reverse(list)
      {item, Enum.reverse(tail)}
    end

    defupdate push(item, list) do
      Enum.reverse([item | list])
    end

    defupdate update_with_timeout(item, list, timeout: 500) do
      Process.sleep(1000)
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

    defcall match_on_binary("string", state) do
      {:reply, "matched string", state}
    end

    defcall match_on_integer(42, state) do
      {:reply, "the answer to the question", state}
    end

    defcall match_with_guard(%{name: name, other: other}, state)
            when is_binary(name) or is_nil(name) do
      {:reply, {name, other}, state}
    end

    defcall func_with_guard(atom, state) when is_atom(atom) do
      {:reply, atom, state}
    end

    defcall pattern_match(%{name: name}, state) do
      pattern_match(name, state)
    end

    defcall pattern_match(name, state) do
      {:reply, name, state}
    end

    defcall pattern_match_state(id, %{name: name} = state) do
      {:reply, {id, name}, state}
    end

    defcall pattern_match_state(id, %{age: age} = state) do
      {:reply, {id, age}, state}
    end

    defcall pattern_match_state(id, state) do
      {:reply, {id, state}, state}
    end

    defcall call_with_timeout(id, state, timeout: 500) do
      Process.sleep(1_000)
      {:reply, {id, state}, state}
    end

    defcast cast_with_state(a, list) do
      [a | list]
    end

    defcontinue(:do_continue, _state) do
      {:noreply, :did_continue}
    end

    definfo({:info_then_continue, new_state}, _state) do
      {:noreply, new_state, {:continue, :do_continue}}
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

  test "call with binary match" do
    assert TestModule.Server.match_on_binary("string", nil) == {:reply, "matched string", nil}

    {:ok, pid} = TestModule.start_link(nil)
    assert TestModule.match_on_binary(pid, "string") == "matched string"
  end

  test "call with integer match" do
    assert TestModule.Server.match_on_integer(42, nil) ==
             {:reply, "the answer to the question", nil}

    {:ok, pid} = TestModule.start_link(nil)
    assert TestModule.match_on_integer(pid, 42) == "the answer to the question"
  end

  test "call with match and guard" do
    assert TestModule.Server.match_with_guard(%{name: "name", other: "other"}, nil) ==
             {:reply, {"name", "other"}, nil}

    assert_raise FunctionClauseError, fn ->
      TestModule.Server.match_with_guard(%{name: :name}, nil)
    end

    {:ok, pid} = TestModule.start_link(nil)
    assert TestModule.match_with_guard(pid, %{name: "name", other: "other"}) == {"name", "other"}

    assert_raise FunctionClauseError, fn ->
      TestModule.match_with_guard(pid, %{name: :name})
    end
  end

  test "call with guard" do
    assert TestModule.Server.func_with_guard(:atom, nil) == {:reply, :atom, nil}

    assert_raise FunctionClauseError, fn ->
      TestModule.Server.func_with_guard("binary", nil)
    end

    {:ok, pid} = TestModule.start_link(nil)
    assert TestModule.func_with_guard(pid, :atom) == :atom

    assert_raise FunctionClauseError, fn ->
      TestModule.func_with_guard(pid, "binary")
    end
  end

  test "call with pattern match" do
    assert TestModule.Server.pattern_match(%{name: "Test name"}, nil) ==
             {:reply, "Test name", nil}

    assert TestModule.Server.pattern_match("Test name", nil) == {:reply, "Test name", nil}

    {:ok, pid} = TestModule.start_link(nil)
    assert TestModule.pattern_match(pid, %{name: "Test name"}) == "Test name"
    assert TestModule.pattern_match(pid, "Test name") == "Test name"
  end

  test "call with pattern match on state" do
    assert TestModule.Server.pattern_match_state(13, %{name: "Test name"}) ==
             {:reply, {13, "Test name"}, %{name: "Test name"}}

    assert TestModule.Server.pattern_match_state(13, %{age: 21}) == {:reply, {13, 21}, %{age: 21}}

    assert TestModule.Server.pattern_match_state(13, "Test name") ==
             {:reply, {13, "Test name"}, "Test name"}

    {:ok, pid} = TestModule.start_link(%{name: "Test name"})
    assert TestModule.pattern_match_state(pid, 13) == {13, "Test name"}
    {:ok, pid} = TestModule.start_link(%{age: 21})
    assert TestModule.pattern_match_state(pid, 13) == {13, 21}
    {:ok, pid} = TestModule.start_link("Test name")
    assert TestModule.pattern_match_state(pid, 13) == {13, "Test name"}
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

    test "set a timeout" do
      assert {:reply, {13, :state}, :state} = TestModule.Server.call_with_timeout(13, :state)

      {:ok, pid} = TestModule.start_link()
      assert catch_exit(TestModule.call_with_timeout(pid, 13))
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
      assert :state = TestModule.Server.get_state(:state)

      {:ok, pid} = TestModule.start_link(:state)
      assert TestModule.get_state(pid) == :state
    end

    test "update and return a value" do
      assert {2, [1]} = TestModule.Server.pop([1, 2])

      {:ok, pid} = TestModule.start_link([1, 2, 3])
      assert TestModule.pop(pid) == 3
      assert TestModule.get_state(pid) == [1, 2]
    end

    test "with timeout" do
      {:ok, pid} = TestModule.start_link(:state)
      assert catch_exit(TestModule.get_with_timeout(pid))
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

    test "with timeout" do
      {:ok, pid} = TestModule.start_link([1])
      assert catch_exit(TestModule.update_with_timeout(pid, 2))
    end
  end

  :otp_release
  |> :erlang.system_info()
  |> to_string()
  |> Integer.parse()
  |> case do
    {version, _} -> version >= 21
    _ -> false
  end
  |> if do
    describe "defcontinue/2" do
      test "it generates a handle_continue callback" do
        assert {:noreply, :did_continue} = TestModule.Server.continue(:do_continue, nil)

        {:ok, pid} = TestModule.start_link([1])
        send(pid, {:info_then_continue, :info})
        assert TestModule.get_state(pid) == :did_continue
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
