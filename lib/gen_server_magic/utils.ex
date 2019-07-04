defmodule GenServerMagic.Utils do
  def remove_names_not_in_when(args, when_clause) do
    required_names = if when_clause, do: Enum.uniq(names_in_when(when_clause)), else: []
    remove_names_from_args(args, required_names)
  end

  defp remove_names_from_args([], _required_names), do: []

  defp remove_names_from_args([{:__aliases__, meta, arguments} | args], required_names) do
    [{:__aliases__, meta, arguments} | remove_names_from_args(args, required_names)]
  end

  defp remove_names_from_args([{name, meta, arguments} | args], required_names)
       when is_list(arguments) do
    [
      {name, meta, remove_names_from_args(arguments, required_names)}
      | remove_names_from_args(args, required_names)
    ]
  end

  defp remove_names_from_args([{key, {name, meta, context}} = arg | args], required_names)
       when is_atom(context) do
    if name in required_names do
      [arg | remove_names_from_args(args, required_names)]
    else
      [{key, {:_, meta, context}} | remove_names_from_args(args, required_names)]
    end
  end

  defp remove_names_from_args([{_, _, context} = arg | args], required_names)
       when is_atom(context) do
    [arg | remove_names_from_args(args, required_names)]
  end

  defp remove_names_from_args([arg | args], required_names) do
    [arg | remove_names_from_args(args, required_names)]
  end

  defp names_in_when({name, _, context}) when is_atom(context), do: [name]

  defp names_in_when({_, _, args}) when is_list(args) do
    names_in_when(args)
  end

  defp names_in_when([]), do: []

  defp names_in_when([arg | args]) do
    names_in_when(arg) ++ names_in_when(args)
  end

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

  def normalize_argument({name, _metadata, other} = arg, pos, _module)
      when is_atom(name) and is_atom(other) do
    name_ignored_argument(arg, pos)
  end

  def normalize_argument({name, _metadata, _other} = arg, pos, _module)
      when is_atom(name) and name in [:=, :\\] do
    name_ignored_argument(arg, pos)
  end

  def normalize_argument(arg, pos, module) do
    {:=, [], [arg, {name_arg(pos), [], module}]}
  end

  defp name_ignored_argument({arg_name, metadata, other} = arg, pos)
       when is_atom(arg_name) and is_atom(other) do
    name_ignored_argument({to_string(arg_name), metadata, other}, arg, pos)
  end

  defp name_ignored_argument(arg, _pos), do: arg

  defp name_ignored_argument({<<"_", _rest::binary>>, metadata, other}, _original_arg, pos) do
    {name_arg(pos), metadata, other}
  end

  defp name_ignored_argument({_arg_name, _metadata, _other}, original_arg, _pos) do
    original_arg
  end

  @doc ~S"""
  Returns only named arguments (excludes pattern matching and default values)
  `(%{}, arg \\ [])` becomes `(gsm_arg0, arg)`
  """
  def name_only_arguments(args, module) do
    args
    |> normalize_arguments(module)
    |> strip_expanded_arguments
    |> strip_optional_arguments
  end

  @doc ~S"""
  Gives every argument a unique name replacing existing named arguments
  `(%{}, arg \\ [])` becomes `(%{} = gsm_arg0, gsm_arg1 \\ [])`
  """
  def rename_arguments(args, module) do
    args
    |> normalize_arguments(module)
    |> name_arguments
  end

  defp name_arguments(args, acc \\ [])
  defp name_arguments([], acc), do: Enum.reverse(acc)

  defp name_arguments([arg | tail], acc),
    do: name_arguments(tail, [name_arg(arg, length(acc)) | acc])

  @doc ~S"""
  Removes the metadata from an argument AST

  `{:var, [line: 42], MyModule}` becomes `{:var, [], MyModule}`
  """
  def strip_metadata(args, acc \\ [])
  def strip_metadata([], acc), do: Enum.reverse(acc)
  def strip_metadata([arg | tail], acc), do: strip_metadata(tail, [strip_arg(arg) | acc])

  def strip_arg(atom) when is_atom(atom), do: atom
  def strip_arg(list) when is_list(list), do: strip_metadata(list)
  def strip_arg({name, arg}) when is_atom(name), do: {name, strip_arg(arg)}
  def strip_arg({name, _metadata, other}), do: {name, [], strip_arg(other)}
  def strip_arg(arg), do: arg

  @doc ~S"""
  Names an argument based on its position
  """
  def name_arg(pos) do
    :"gsm_arg#{pos}"
  end

  defp name_arg({:=, metadata, [val, arg]}, pos) do
    {:=, metadata, [val, name_arg(arg, pos)]}
  end

  defp name_arg({:\\, metadata, [arg, var]}, pos) do
    {:\\, metadata, [name_arg(arg, pos), var]}
  end

  defp name_arg({_arg, metadata, other}, pos) do
    {name_arg(pos), metadata, other}
  end

  defp name_arg(arg, pos) do
    {:=, [], [arg, name_arg(pos)]}
  end
end
