Code.require_file "../test_helper.exs", __FILE__

defmodule MsgPackTest do
  use ExUnit.Case

  use Proper.Properties
  alias :proper_types, as: T

  require MsgPack

  defp positive_fixnum, do: choose(0, 127)
  defp negative_fixnum, do: choose(-32, -1)
  defp int8, do: choose(-0x80+1, -33)
  defp int16, do: choose(-0x8000, -0x80)
  defp int32, do: choose(-0x80000000, -0x8000)
  defp int64, do: choose(-8000000000000000, -0x80000000)
  defp uint8, do: choose(128, 0xff)
  defp uint16, do: choose(0xff + 1, 0xffff)
  defp uint32, do: choose(0xffff + 1, 0xffffffff)
  defp uint64, do: choose(0xffffffff + 1, 0xffffffffffffffff)

  defp fix_raw do
    let sz = choose(0, 31) do
      let binary = binary(sz), do: binary
    end
  end

  defp raw16 do
    let sz = uint16 do
      let binary = binary(sz), do: binary
    end
  end

  defp raw32 do
    let sz = uint32 do
      let binary = binary(sz), do: binary
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
       sized(_, msgpack_array(div(size, 2))),
       sized(_, msgpack_map(div(size, 2)))
    ])
  end
  defp msgpack_array(size) do
    list(msgpack(size))
  end
  defp msgpack_map(size) do
    let kv = list({msgpack(size), msgpack(size)}), do: MsgPack.Map.from_list(kv)
  end

  defmacrop qc(do: body) do
    quote do
      f =
      fn() ->
        unquote(body)
      end
      assert Proper.quickcheck(f.(), numtests: 100) == true
    end
  end

  test "pack -> unpack equality" do
    qc do
      forall val in msgpack do
        {value, ""} =
        MsgPack.pack(val) />
        MsgPack.packed_to_binary />
        MsgPack.unpack
        value == val
      end
    end
  end

  test "pack -> next -> unpack equality" do
    qc do
      forall val in msgpack do
        {bin, ""} =
        MsgPack.pack(val) />
        MsgPack.packed_to_binary />
        MsgPack.next
        {value, ""} = MsgPack.unpack(bin)
        value == val
      end
    end
  end

  test "atom_nil macro" do
    assert match?(MsgPack.atom_nil, MsgPack.pack(nil))
  end

  test "atom_false macro" do
    assert match?(MsgPack.atom_false, MsgPack.pack(false))
  end

  test "atom_true macro" do
    assert match?(MsgPack.atom_true, MsgPack.pack(true))
  end

  test "float32 macro" do
    # TODO
  end

  test "float64 macro" do
    qc do
      forall val in float do
        match?(MsgPack.float64, MsgPack.pack(val))
      end
    end
  end

  test "uint7 macro" do
    qc do
      forall val in positive_fixnum do
        match?(MsgPack.uint7, MsgPack.pack(val))
      end
    end
  end

  test "uint8 macro" do
    qc do
      forall val in uint8 do
        match?(MsgPack.uint8, MsgPack.pack(val))
      end
    end
  end

  test "uint16 macro" do
    qc do
      forall val in uint16 do
        match?(MsgPack.uint16, MsgPack.pack(val))
      end
    end
  end

  test "uint32 macro" do
    qc do
      forall val in uint32 do
        match?(MsgPack.uint32, MsgPack.pack(val))
      end
    end
  end

  test "uint64 macro" do
    qc do
      forall val in uint64 do
        match?(MsgPack.uint64, MsgPack.pack(val))
      end
    end
  end

  test "int5 macro" do
    qc do
      forall val in negative_fixnum do
        match?(MsgPack.int5, MsgPack.pack(val))
      end
    end
  end

  test "int8 macro" do
    qc do
      forall val in int8 do
        match?(MsgPack.int8, MsgPack.pack(val))
      end
    end
  end

  test "int16 macro" do
    qc do
      forall val in int16 do
        match?(MsgPack.int16, MsgPack.pack(val))
      end
    end
  end

  test "int32 macro" do
    qc do
      forall val in int32 do
        match?(MsgPack.int32, MsgPack.pack(val))
      end
    end
  end

  test "int64 macro" do
    qc do
      forall val in int64 do
        match?(MsgPack.int64, MsgPack.pack(val))
      end
    end
  end

  test "fix_raw macro" do
    qc do
      forall val in fix_raw do
        len = byte_size(val)
        match?(MsgPack.fix_raw(len: ^len), MsgPack.pack(val))
      end
    end
  end

  test "raw16 macro" do
    qc do
      forall val in raw16 do
        len = byte_size(val)
        match?(MsgPack.raw16(len: ^len), MsgPack.pack(val))
      end
    end
  end

  test "raw32 macro" do
    qc do
      forall val in raw32 do
        len = byte_size(val)
        match?(MsgPack.raw32(len: ^len), MsgPack.pack(val))
      end
    end
  end

  test "fix_array macro" do
    assert MsgPack.fix_array(len: 3) = MsgPack.pack([1,2,3]) /> MsgPack.packed_to_binary
  end

  test "array16 macro" do
    assert MsgPack.array16(len: 16) = MsgPack.pack(List.duplicate(0, 16)) /> MsgPack.packed_to_binary
  end

  test "array32 macro" do
    assert MsgPack.array32(len: 0x10000) = MsgPack.pack(List.duplicate(0, 0x10000)) /> MsgPack.packed_to_binary
  end

  test "fix_map macro" do
    assert MsgPack.fix_map([len: 3]) = MsgPack.pack(MsgPack.Map.from_list([{1,1},{2,2},{3,3}])) /> MsgPack.packed_to_binary
  end

  test "map16 macro" do
    assert MsgPack.map16(len: 16) = MsgPack.pack(MsgPack.Map.from_list(List.duplicate({0,0}, 16))) /> MsgPack.packed_to_binary
  end

  test "map32 macro" do
    assert MsgPack.map32(len: 0x10000) = MsgPack.pack(MsgPack.Map.from_list(List.duplicate({0,0}, 0x10000))) /> MsgPack.packed_to_binary
  end

end
