Code.require_file "../test_helper.exs", __FILE__

defmodule MsgPackTest do
  use ExUnit.Case

  use Proper.Properties
  alias :proper_types, as: T

  def msgpack, do: msgpack(4)
  def msgpack(0) do
    oneof([
       T.integer,
       T.float,
       T.binary
    ])
  end
  def msgpack(size) do
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
