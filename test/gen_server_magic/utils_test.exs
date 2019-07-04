defmodule GenServerMagic.UtilsTest do
  use ExUnit.Case, async: true
  require GenServerMagic.Utils
  alias GenServerMagic.Utils

  defmodule TestStruct do
    defstruct name: nil
  end

  describe "remove_names_not_in_when/2" do
    test "it removes the variable names not used in the when clause" do
      when_clause =
        {:or, [line: 75],
         [
           {:is_binary, [line: 75], [{:name, [line: 75], nil}]},
           {:is_nil, [line: 75], [{:name, [line: 75], nil}]}
         ]}

      args = [
        {:=, [],
         [
           {:%{}, [line: 75], [name: {:name, [line: 75], nil}, other: {:other, [line: 75], nil}]},
           {:gsm_arg0, [], GenServerMagic}
         ]},
        {:gsm_arg1, [], GenServerMagic}
      ]

      expected_args = [
        {:=, [],
         [
           {:%{}, [line: 75], [name: {:name, [line: 75], nil}, other: {:_, [line: 75], nil}]},
           {:gsm_arg0, [], GenServerMagic}
         ]},
        {:gsm_arg1, [], GenServerMagic}
      ]

      assert Utils.remove_names_not_in_when(args, when_clause) == expected_args
    end
  end

  describe "strip_optional_arguments/1" do
    test "it removes the default value from optional arguments" do
      {:def, _, [{:func, _, args}]} =
        quote do
          def func(one, two \\ [], %{} = three, "string", 42, :atom, {:one, :two, :three})
        end

      {:def, _, [{:func, _, expected_args}]} =
        quote do
          def func(one, two, %{} = three, "string", 42, :atom, {:one, :two, :three})
        end

      assert Utils.strip_optional_arguments(args) == expected_args
    end
  end

  describe "strip_expanded_arguments/1" do
    test "it removes the pattern match from expanded arguments" do
      {:def, _, [{:func, _, args}]} =
        quote do
          def func(
                %{} = one,
                [] = two,
                three,
                four \\ nil,
                "string",
                42,
                :atom,
                {:one, :two, :three},
                abs(-1)
              )
        end

      {:def, _, [{:func, _, expected_args}]} =
        quote do
          def func(
                one,
                two,
                three,
                four \\ nil,
                "string",
                42,
                :atom,
                {:one, :two, :three},
                abs(-1)
              )
        end

      assert Utils.strip_expanded_arguments(args) == expected_args
    end
  end

  describe "normalize_arguments/2" do
    test "it adds names to all unnamed arguments" do
      {:def, _, [{:func, _, args}]} =
        quote do
          def func(
                %{},
                two,
                [],
                four \\ [],
                %{name: name} = five,
                [arg: :value] = six,
                %TestStruct{name: :name},
                _eight,
                "string",
                42,
                :atom,
                {:one, :two, :three},
                abs(-1)
              )
        end

      {:def, _, [{:func, _, expected_args}]} =
        quote do
          def func(
                %{} = gsm_arg0,
                two,
                [] = gsm_arg2,
                four \\ [],
                %{name: name} = five,
                [arg: :value] = six,
                %TestStruct{name: :name} = gsm_arg6,
                gsm_arg7,
                "string" = gsm_arg8,
                42 = gsm_arg9,
                :atom = gsm_arg10,
                {:one, :two, :three} = gsm_arg11,
                abs(-1) = gsm_arg12
              )
        end

      assert Utils.normalize_arguments(args, __MODULE__) == expected_args
    end
  end

  describe "rename_arguments/2" do
    test "replaces all argument names" do
      {:def, _, [{:func, _, args}]} =
        quote do
          def func(
                %{},
                two,
                [],
                four \\ [],
                %{name: name} = five,
                [arg: :value] = six,
                %TestStruct{name: :name},
                _eight,
                "string",
                42,
                :atom,
                {:one, :two, :three},
                abs(-1)
              )
        end

      {:def, _, [{:func, _, expected_args}]} =
        quote do
          def func(
                %{} = gsm_arg0,
                gsm_arg1,
                [] = gsm_arg2,
                gsm_arg3 \\ [],
                %{name: name} = gsm_arg4,
                [arg: :value] = gsm_arg5,
                %TestStruct{name: :name} = gsm_arg6,
                gsm_arg7,
                "string" = gsm_arg8,
                42 = gsm_arg9,
                :atom = gsm_arg10,
                {:one, :two, :three} = gsm_arg11,
                abs(-1) = gsm_arg12
              )
        end

      assert Utils.rename_arguments(args, __MODULE__) == expected_args
    end
  end

  describe "strip_metadata/2" do
    test "removes metadata from arguments" do
      assert Utils.strip_metadata([
               {:arg0, [line: 42], __MODULE__},
               {:\\, [line: 42], [{:arg1, [line: 42], __MODULE__}, []]}
             ]) ==
               [
                 {:arg0, [], __MODULE__},
                 {:\\, [], [{:arg1, [], __MODULE__}, []]}
               ]
    end
  end

  test "name_arg/1" do
    assert Utils.name_arg(0) == :gsm_arg0
  end
end
