defmodule GenServerMagic.Utils do
  @doc ~S"""
  Removes the default value from optional arguments
  """
  def strip_optional_arguments(args, acc \\ [])
  def strip_optional_arguments([], acc), do: Enum.reverse(acc)

  def strip_optional_arguments([{:\\, _, [arg, _value]} | tail], acc),
    do: strip_optional_arguments(tail, [arg | acc])

  def strip_optional_arguments([arg | tail], acc), do: strip_optional_arguments(tail, [arg | acc])

  @doc ~S"""
  Removes the pattern from named arguemnts

  `(%{} = map)` becomes `(map)`
  """
  def strip_expanded_arguments(args, acc \\ [])
  def strip_expanded_arguments([], acc), do: Enum.reverse(acc)

  def strip_expanded_arguments([{:=, _, [_arg, arg]} | tail], acc),
    do: strip_expanded_arguments(tail, [arg | acc])

  def strip_expanded_arguments([arg | tail], acc), do: strip_expanded_arguments(tail, [arg | acc])

  @doc ~S"""
  Adds names to all unnamed args

  `(%{})` becomes `(%{} = gsm_arg0)`
  """
  def normalize_arguments(args, module, acc \\ [])
  def normalize_arguments([], _module, acc), do: Enum.reverse(acc)

  def normalize_arguments([arg | tail], module, acc) do
    normalize_arguments(tail, module, [normalize_argument(arg, length(acc), module) | acc])
  end

  def normalize_argument({:%{}, _metadata, _args} = arg, pos, module) do
    {:=, [], [arg, {name_arg(pos), [], module}]}
  end

  def normalize_argument({:%, _metadata, _args} = arg, pos, module) do
    {:=, [], [arg, {name_arg(pos), [], module}]}
  end

  def normalize_argument([] = arg, pos, module) do
    {:=, [], [arg, {name_arg(pos), [], module}]}
  end

  def normalize_argument(arg, _pos, _module) do
    # IO.inspect(arg, label: "arg")
    arg
  end

  def name_only_arguments(args, module) do
    args
    |> normalize_arguments(module)
    |> strip_expanded_arguments
    |> strip_optional_arguments
  end

  @doc ~S"""
  Names an argument based on its position
  """
  def name_arg(pos) do
    :"gsm_arg#{pos}"
  end
end
