defmodule GenServerMagic.UtilsTest do
  use ExUnit.Case, async: true
  require GenServerMagic.Utils
  alias GenServerMagic.Utils

  defmodule TestStruct do
    defstruct name: nil
  end

  describe "strip_optional_arguments/1" do
    test "it removes the default value from optional arguments" do
      {:def, _, [{:func, _, args}]} =
        quote do
          def func(one, two \\ [], %{} = three)
        end

      {:def, _, [{:func, _, expected_args}]} =
        quote do
          def func(one, two, %{} = three)
        end

      assert Utils.strip_optional_arguments(args) == expected_args
    end
  end

  describe "strip_expanded_arguments/1" do
    test "it removes the pattern match from expanded arguments" do
      {:def, _, [{:func, _, args}]} =
        quote do
          def func(%{} = one, [] = two, three, four \\ nil)
        end

      {:def, _, [{:func, _, expected_args}]} =
        quote do
          def func(one, two, three, four \\ nil)
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
                %TestStruct{name: :name}
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
                %TestStruct{name: :name} = gsm_arg6
              )
        end

      assert Utils.normalize_arguments(args, __MODULE__) == expected_args
    end
  end

  describe "name_only_arguments/2" do
    test "it reduces all arguments to named versions" do
      {:def, _, [{:func, _, args}]} =
        quote do
          def func(
                %{},
                two,
                [],
                four \\ [],
                %{name: name} = five,
                [arg: :value] = six,
                %TestStruct{name: :name}
              )
        end

      {:def, _, [{:func, _, expected_args}]} =
        quote do
          def func(gsm_arg0, two, gsm_arg2, four, five, six, gsm_arg6)
        end

      assert Utils.name_only_arguments(args, __MODULE__) == expected_args
    end
  end

  test "name_arg/1" do
    assert Utils.name_arg(0) == :gsm_arg0
  end
end
