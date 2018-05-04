# GenServerMagic

## Installation

Add `gen_server_magic` as a dependency in your project in your `mix.exs` file:

```elixir
def deps do
  [
    {:gen_server_magic, "~> 0.0.1"}
  ]
end
```

## Usage

Buyer beware, this library uses macro magic to transform:

```elixir
defmodule MyModule do
  use GenServerMagic

  defserver do
    require Logger

    defmodule State do
      defstruct counter: nil
    end
  end

  definit(start) do
    {:ok, %State{counter: start}}
  end

  defget get_func(%{counter: counter} = state) do
    {counter, state}
  end

  defupdate update_func(val, state) do
    state
  end

  defcall call_func(arg1, arg2, state) do
    {:reply, :ok, state}
  end

  defcall early_return(arg1, arg2, {from, state}) do
    GenServer.reply(from, :ok)
    Logger.info("Doing work after return")
    {:noreply, state}
  end

  defcast cast_func(arg1, arg2, state) do
    {:noreply, state}
  end

  definfo(:message, state) do
    {:noreply, state}
  end

  defterminate(reason, state) do
    Logger.info("terminating")
    :ok
  end
end
```

into:

```elixir
defmodule MyModule do

  def get_func(pid) do
    GenServer.call(pid, {:get_func, []})
  end

  def update_func(pid, val) do
    GenServer.call(pid, {:update_func, [val]})
  end

  def call_func(pid, arg1, arg2) do
    GenServer.call(pid, {:call_func, [arg1, arg2]})
  end

  def early_return(pid, arg1, arg2) do
    GenServer.call(pid, {:early_return, [arg1, arg2]})
  end

  def cast_func(pid, arg1, arg2) do
    GenServer.cast(pid, {:cast_func, [arg1, arg2]})
  end

  defmodule Server do
    require Logger

    defmodule State do
      defstruct counter: nil
    end

    def init(start) do
      {:ok, %State{counter: start}}
    end

    def get_func(%{counter: counter} = state) do
      {counter, state}
    end

    def update_func(val, state) do
      state
    end

    def call_func(arg1, arg2, state) do
      {:reply, :ok, state}
    end

    def early_return(arg1, arg2, state) do
      GenServer.reply(from, :ok)
      Logger.info("Doing work after return")
      {:noreply, state}
    end

    def cast_func(arg1, arg2, state) do
      {:noreply, state}
    end

    def info(:message, state) do
      {:noreply, state}
    end

    def handle_call({:get_func, []}, _from, state) do
      {retval, new_state} = get_func(state)
      {:reply, retval, new_state}
    end

    def handle_call({:update_func, [val]}, _from, state) do
      {:reply, :ok, update_func(val, state)}
    end

    def handle_call({:early_return, [arg1, arg2]}, from, state) do
      early_return(arg1, arg2, {from, state})
    end

    def handle_cast({:cast_func, [arg1, arg2]}, state) do
      cast_func(arg1, arg2, state)
    end

    def handle_info(:message, state) do
      info(:message, state)
    end

    def terminate(reason, state) do
      Logger.info("terminating")
      :ok
    end
  end
end
```
