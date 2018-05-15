defmodule GenServerMagic do
  @moduledoc """
  Documentation for GenServerMagic.
  """
  defmacro __using__(opts) do
    # IO.puts("__CALLER__: #{inspect(__CALLER__)}")
    server_module = Keyword.get(opts, :server_module, :"#{__CALLER__.module}.Server")
    registered_name = Keyword.get(opts, :registered_name)

    quote do
      @registered_name unquote(registered_name)
      @server_module unquote(server_module)
      Module.register_attribute(__MODULE__, :gen_server_methods, accumulate: true)
      Module.register_attribute(__MODULE__, :api_methods, accumulate: true)
      @gen_server_methods {:use, GenServer}
      @before_compile unquote(__MODULE__)
      @compile :nowarn_unused_vars

      import Kernel, except: [defp: 2]
      import GenServerMagic

      def child_spec(opts) do
        %{
          id: @server_module,
          start: {__MODULE__, :start_link, [opts]},
          type: :worker,
          restart: :permanent,
          shutdown: 500
        }
      end

      def start_link(args \\ nil, opts \\ []) do
        opts = Keyword.merge([name: @registered_name], opts)
        GenServer.start_link(@server_module, args, opts)
      end

      defoverridable child_spec: 1, start_link: 2
    end
  end

  defmacro __before_compile__(_env) do
    # IO.puts("env: #{inspect(env, limit: :infinity)}")

    implementation =
      quote do
        @gen_server_methods
        |> Enum.reverse()
        |> Enum.map(fn
          {:server, func_body} ->
            func_body

          {:call, func_name, args, {from, state}, func_body, nil} ->
            quote do
              # def func_name(a, b, {from, state})
              def unquote(func_name)(unquote_splicing(args), {unquote(from), unquote(state)}),
                do: unquote(func_body)
            end

          {:call, func_name, args, {from, state}, func_body, when_clause} ->
            quote do
              # def func_name(a, b, {from, state})
              def unquote(func_name)(unquote_splicing(args), {unquote(from), unquote(state)})
                  when unquote(when_clause),
                  do: unquote(func_body)
            end

          {:call, func_name, args, state, func_body, nil} ->
            quote do
              # def func_name(a, b, state)
              def unquote(func_name)(unquote_splicing(args), unquote(state)),
                do: unquote(func_body)
            end

          {:call, func_name, args, state, func_body, when_clause} ->
            quote do
              # def func_name(a, b, state)
              def unquote(func_name)(unquote_splicing(args), unquote(state))
                  when unquote(when_clause),
                  do: unquote(func_body)
            end

          {:defp, func_name, args, func_body} ->
            quote do
              # defp func_name(a, b)
              defp unquote(func_name)(unquote_splicing(args || [])), do: unquote(func_body)
            end

          {:get, func_name, args, state, func_body, nil} ->
            quote do
              # def func_name(a, b, state)
              def unquote(func_name)(unquote_splicing(args), unquote(state)),
                do: unquote(func_body)
            end

          {:get, func_name, args, state, func_body, when_clause} ->
            quote do
              # def func_name(a, b, state)
              def unquote(func_name)(unquote_splicing(args), unquote(state))
                  when unquote(when_clause),
                  do: unquote(func_body)
            end

          {:update, func_name, args, state, func_body, nil} ->
            quote do
              # def func_name(a, b, state)
              def unquote(func_name)(unquote_splicing(args), unquote(state)),
                do: unquote(func_body)
            end

          {:update, func_name, args, state, func_body, when_clause} ->
            quote do
              # def func_name(a, b, state)
              def unquote(func_name)(unquote_splicing(args), unquote(state))
                  when unquote(when_clause),
                  do: unquote(func_body)
            end

          {:cast, func_name, args, state, func_body, nil} ->
            quote do
              # def func_name(a, b, state)
              def unquote(func_name)(unquote_splicing(args), unquote(state)),
                do: unquote(func_body)
            end

          {:cast, func_name, args, state, func_body, when_clause} ->
            quote do
              # def func_name(a, b, state)
              def unquote(func_name)(unquote_splicing(args), unquote(state))
                  when unquote(when_clause),
                  do: unquote(func_body)
            end

          {:info, message, state, func_body} ->
            quote do
              # def info(message, state)
              def info(unquote(message), unquote(state)), do: unquote(func_body)
            end

          _ ->
            nil
        end)
        |> Enum.filter(& &1)
        |> Enum.uniq()
      end

    server =
      quote do
        @gen_server_methods
        |> Enum.reverse()
        |> Enum.map(fn
          {:use, module} ->
            {:use,
             quote do
               @compile :nowarn_unused_vars
               @moduledoc false
               use unquote(module)
             end}

          {:init, args, func_body} ->
            {:init,
             quote do
               def init(unquote_splicing(args)), do: unquote(func_body)
             end}

          {:call, func_name, args, {from, state}, func_body, _when_clause} ->
            n_args = GenServerMagic.normalize_arguments(args)
            n_from = GenServerMagic.normalize_argument(from, 1)
            n_state = GenServerMagic.normalize_argument(state, 1)

            {{:call, func_name, length(args), 2},
             quote do
               # def handle_call({fun_name, [a, b]}, from, state) do
               #   func_name(a, b, {from, state})
               # end
               def handle_call(
                     {unquote(func_name), unquote(n_args)},
                     unquote(n_from),
                     unquote(n_state)
                   ) do
                 unquote(func_name)(
                   unquote_splicing(n_args),
                   {unquote(n_from), unquote(state)}
                 )
               end
             end}

          {:call, func_name, args, state, func_body, _when_clause} ->
            n_args = GenServerMagic.normalize_arguments(args)
            n_state = GenServerMagic.normalize_argument(state, 1)

            {{:call, func_name, length(args), 1},
             quote do
               # def handle_call({fun_name, [a, b]}, from, state) do
               #   func_name(a, b, state)
               # end
               def handle_call(
                     {unquote(func_name), unquote(n_args)},
                     _from,
                     unquote(n_state)
                   ) do
                 unquote(func_name)(
                   unquote_splicing(n_args),
                   unquote(n_state)
                 )
               end
             end}

          {:get, func_name, args, state, func_body, _when_clause} ->
            n_args = GenServerMagic.normalize_arguments(args)
            n_state = GenServerMagic.normalize_argument(state, 1)

            {{:call, func_name, length(args), 1},
             quote do
               # def handle_call({fun_name, [a, b]}, from, state) do
               #   func_name(a, b, state)
               # end
               def handle_call(
                     {unquote(func_name), unquote(n_args)},
                     _from,
                     unquote(n_state)
                   ) do
                 {reply, new_state} =
                   unquote(func_name)(
                     unquote_splicing(n_args),
                     unquote(n_state)
                   )

                 {:reply, reply, new_state}
               end
             end}

          {:update, func_name, args, state, func_body, _when_clause} ->
            n_args = GenServerMagic.normalize_arguments(args)
            n_state = GenServerMagic.normalize_argument(state, 1)

            {{:call, func_name, length(args), 1},
             quote do
               # def handle_call({fun_name, [a, b]}, from, state) do
               #   func_name(a, b, state)
               # end
               def handle_call({unquote(func_name), unquote(n_args)}, _from, unquote(n_state)) do
                 {:reply, :ok, unquote(func_name)(unquote_splicing(n_args), unquote(n_state))}
               end
             end}

          {:cast, func_name, args, state, func_body, _when_clause} ->
            n_args = GenServerMagic.normalize_arguments(args)
            n_state = GenServerMagic.normalize_argument(state, 1)

            {{:cast, func_name, length(args)},
             quote do
               # def handle_cast({fun_name, [a, b]}, state) do
               #   func_name(a, b, state)
               # end
               def handle_cast({unquote(func_name), unquote(n_args)}, unquote(n_state)) do
                 case unquote(func_name)(
                        unquote_splicing(n_args),
                        unquote(n_state)
                      ) do
                   {:noreply, new_state} -> {:noreply, new_state}
                   {:noreply, new_state, timeout} -> {:noreply, new_state, timeout}
                   {:stop, reason, new_state} -> {:stop, reason, new_state}
                   new_state -> {:noreply, new_state}
                 end
               end
             end}

          {:info, message, state, func_body} ->
            n_state = GenServerMagic.normalize_argument(state, 1)

            {:info,
             quote do
               # def handle_info({fun_name, a, b}, state) do
               #   func_name(a, b, state)
               # end
               def handle_info(message, unquote(n_state)) do
                 case info(message, unquote(n_state)) do
                   {:noreply, new_state} -> {:noreply, new_state}
                   {:noreply, new_state, timeout} -> {:noreply, new_state, timeout}
                   {:stop, reason, new_state} -> {:stop, reason, new_state}
                   new_state -> {:noreply, new_state}
                 end
               end
             end}

          {:terminate, reason, state, func_body} ->
            {{:terminate, reason},
             quote do
               # def terminate(reason, state)
               def terminate(unquote(reason), unquote(state)), do: unquote(func_body)
             end}

          _ ->
            nil
        end)
        |> Enum.filter(& &1)
        |> Enum.uniq_by(fn {id, _quoted} -> id end)
        |> Enum.map(fn {_id, quoted} -> quoted end)
      end

    quote do
      # IO.puts(Macro.to_string(Macro.expand(unquote(implementation), __ENV__)))
      # IO.puts(Macro.to_string(Macro.expand(unquote(server), __ENV__)))

      Module.create(
        @server_module,
        unquote([implementation, server]),
        __ENV__
        # Macro.Env.location(__ENV__)
      )
    end
  end

  defmacro defserver(do: func_body) do
    func_body = Macro.escape(func_body)

    quote do
      @gen_server_methods {:server, unquote(func_body)}
    end
  end

  defmacro definit(args, do: func_body) do
    args = Macro.escape([args])
    func_body = Macro.escape(func_body)

    quote do
      @gen_server_methods {:init, unquote(args), unquote(func_body)}
    end
  end

  defmacro defcall({func_name, _context, args}, expr \\ nil) do
    define_callbacks(:call, func_name, args, expr)
  end

  defmacro defget({func_name, _context, args}, expr \\ nil) do
    define_callbacks(:get, func_name, args, expr)
  end

  defmacro defupdate({func_name, _context, args}, expr \\ nil) do
    define_callbacks(:update, func_name, args, expr)
  end

  defmacro defcast({func_name, _context, args}, expr \\ nil) do
    define_callbacks(:cast, func_name, args, expr)
  end

  defmacro definfo(message, state, do: func_body) do
    message = Macro.escape(message)
    state = Macro.escape(state)
    func_body = Macro.escape(func_body)

    quote do
      @gen_server_methods {:info, unquote(message), unquote(state), unquote(func_body)}
    end
  end

  defmacro defterminate(reason, state, do: func_body) do
    reason = Macro.escape(reason)
    state = Macro.escape(state)
    func_body = Macro.escape(func_body)

    quote do
      @gen_server_methods {:terminate, unquote(reason), unquote(state), unquote(func_body)}
    end
  end

  defmacro defp({func_name, _context, args}, do: func_body) do
    args = Macro.escape(args)
    func_body = Macro.escape(func_body)

    quote do
      @gen_server_methods {:defp, unquote(func_name), unquote(args), unquote(func_body)}
    end
  end

  defp define_callbacks(_type, func_name, args, nil) do
    [_state | rest] = Enum.reverse(args)
    local_args = Enum.reverse(rest)

    quote do
      if @registered_name do
        def unquote(func_name)(unquote_splicing(local_args))
      else
        def unquote(func_name)(pid, unquote_splicing(local_args))
      end
    end
  end

  defp define_callbacks(type, func_name, args, do: func_body) do
    {func_name, args, when_clause} =
      case func_name do
        :when ->
          [{func_name, _, args}, when_clause] = args
          {func_name, args, when_clause}

        _other ->
          {func_name, args, nil}
      end

    [state | rest] = Enum.reverse(args)
    local_args = Enum.reverse(rest)
    n_local_args = strip_expanded_arguments(local_args)
    server_args = Macro.escape(strip_optional_arguments(local_args))
    state = Macro.escape(state)
    func_body = Macro.escape(func_body)
    function_call = genserver_function(type)
    escaped_when_clause = Macro.escape(when_clause)
    has_when = !!when_clause

    quoted =
      quote do
        @gen_server_methods {unquote(type), unquote(func_name), unquote(server_args),
                             unquote(state), unquote(func_body), unquote(escaped_when_clause)}

        # Don't write an API function with the same signature (compiler warning)
        unless Enum.find(@api_methods, fn
                 {unquote(func_name), unquote(length(local_args))} -> true
                 _ -> false
               end) do
          @api_methods {unquote(func_name), unquote(length(local_args))}

          if @registered_name do
            if unquote(has_when) do
              def unquote(func_name)(unquote_splicing(n_local_args)) when unquote(when_clause),
                do:
                  GenServer.unquote(function_call)(
                    @registered_name,
                    {unquote(func_name), unquote(normalize_arguments(local_args))}
                  )
            else
              def unquote(func_name)(unquote_splicing(n_local_args)),
                do:
                  GenServer.unquote(function_call)(
                    @registered_name,
                    {unquote(func_name), unquote(normalize_arguments(local_args))}
                  )
            end
          else
            if unquote(has_when) do
              def unquote(func_name)(pid, unquote_splicing(n_local_args))
                  when is_pid(pid) and unquote(when_clause),
                  do:
                    GenServer.unquote(function_call)(
                      pid,
                      {unquote(func_name), unquote(normalize_arguments(local_args))}
                    )
            else
              def unquote(func_name)(pid, unquote_splicing(n_local_args)) when is_pid(pid),
                do:
                  GenServer.unquote(function_call)(
                    pid,
                    {unquote(func_name), unquote(normalize_arguments(local_args))}
                  )
            end
          end
        end
      end

    # IO.inspect(Macro.to_string(Macro.expand(quoted, __ENV__)), label: "quoted")
    quoted
  end

  defp genserver_function(:get), do: :call
  defp genserver_function(:update), do: :call
  defp genserver_function(type), do: type

  defp strip_optional_arguments(args, acc \\ [])
  defp strip_optional_arguments([], acc), do: Enum.reverse(acc)

  defp strip_optional_arguments([{:\\, _, [arg, _value]} | tail], acc),
    do: strip_optional_arguments(tail, [arg | acc])

  defp strip_optional_arguments([arg | tail], acc),
    do: strip_optional_arguments(tail, [arg | acc])

  defp strip_expanded_arguments(args, acc \\ [])
  defp strip_expanded_arguments([], acc), do: Enum.reverse(acc)

  defp strip_expanded_arguments([{:=, _, [_arg, arg]} | tail], acc),
    do: strip_expanded_arguments(tail, [normalize_argument(arg, length(acc)) | acc])

  defp strip_expanded_arguments([arg | tail], acc),
    do: strip_expanded_arguments(tail, [normalize_argument(arg, length(acc)) | acc])

  @doc false
  def normalize_arguments(args, acc \\ [])
  def normalize_arguments([], acc), do: Enum.reverse(acc)

  def normalize_arguments([{:\\, _, [arg, _value]} | tail], acc),
    do: normalize_arguments(tail, [normalize_argument(arg, length(acc)) | acc])

  def normalize_arguments([{:=, _, [_, arg]} | tail], acc),
    do: normalize_arguments(tail, [normalize_argument(arg, length(acc)) | acc])

  def normalize_arguments([arg | tail], acc),
    do: normalize_arguments(tail, [normalize_argument(arg, length(acc)) | acc])

  @doc false
  def normalize_argument({:\\, _, _} = arg, _pos), do: arg
  def normalize_argument({:=, _, [_, arg]}, pos), do: normalize_argument(arg, pos)

  def normalize_argument({:%, _, [{:__aliases__, _, _}, _arg]} = arg, pos),
    do: {:=, [], [arg, {:"arg#{pos}", [], nil}]}

  def normalize_argument({:%{}, _, _} = arg, pos), do: {:=, [], [arg, {:"arg#{pos}", [], nil}]}

  def normalize_argument({arg, context, nil}, pos) when is_atom(arg) and is_list(context),
    do: normalize_argument(Atom.to_string(arg), arg, context, pos)

  def normalize_argument(arg, pos) when is_tuple(arg) do
    arg
    |> Tuple.to_list()
    |> Enum.map(&normalize_argument(&1, pos))
    |> List.to_tuple()
  end

  defp normalize_argument(<<"_", arg::binary>>, _original_arg, context, _pos),
    do: {String.to_atom(arg), context, nil}

  defp normalize_argument(_arg, original_arg, context, _pos), do: {original_arg, context, nil}
end
