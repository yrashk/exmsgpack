Code.require_file "../test_helper.exs", __FILE__

defmodule MsgPackTest do
  use ExUnit.Case

  use Proper.Properties
  alias :proper_types, as: T


  defp positive_fixnum, do: choose(0, 127)
  defp negative_fixnum, do: choose(-32, -1)
  defp int8, do: choose(-0x80, 0)
  defp int16, do: choose(-0x8000, -0x80)
  defp int32, do: choose(-0x80000000, -0x8000)
  defp int64, do: choose(-8000000000000000, -80000000)
  defp uint8, do: choose(0, 0xff)
  defp uint16, do: choose(0xff, 0xffff)
  defp uint32, do: choose(0xffff, 0xffffffff)
  defp uint64, do: choose(0xffffffff, 0xffffffffffffffff)

  defp fix_raw do
    let integer = choose(0, 31) do
      let binary = binary(integer), do: binary
    end
  end

  defp raw16 do
    let integer = uint16 do
      let binary = binary(integer), do: binary
    end
  end

  defp raw32 do
    let integer = uint32 do
      let binary = binary(integer), do: binary
    end
  end

  defp heavy_msgpack do
    oneof([raw16, raw32])
  end
  defp msgpack, do: frequency([{10, msgpack(4)}, {1, heavy_msgpack}])
  defp msgpack(0) do
    oneof([
       positive_fixnum, negative_fixnum,
       int8, int16, int32, int64,
       uint8, uint16, uint32, uint64,
       float, :nil, boolean, fix_raw])
  end
  defp msgpack(size) do
    oneof([
       msgpack(0),
       sized(_, list(msgpack(div(size, 2)))),
       sized(_,
        let kv = list({msgpack(div(size, 2)), msgpack(div(size, 2))}), do: MsgPack.Map.from_list(kv))
    ])
  end

  test "pack -> unpack equality" do
    f =
    fn() ->
      forall val in msgpack do
        {value, ""} =
        MsgPack.pack(val) />
        MsgPack.packed_to_binary />
        MsgPack.unpack
        value == val
      end
    end
    assert Proper.quickcheck(f.(), numtests: 100) == true
  end

  test "pack -> next -> unpack equality" do
    f =
    fn() ->
      forall val in msgpack do
        {bin, ""} =
        MsgPack.pack(val) />
        MsgPack.packed_to_binary />
        MsgPack.next
        {value, ""} = MsgPack.unpack(bin)
        value == val
      end
    end
    assert Proper.quickcheck(f.(), numtests: 100) == true
  end

end
