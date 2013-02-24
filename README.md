# MsgPack for Elixir

MsgPack (http://msgpack.org) implementation for Elixir

## Examples

```elixir
iex(1)> b = MsgPack.pack([1,2,["3", MsgPack.Map.from_list([{"a","b"}])]])
<<147,1,2,146,161,51,129,161,97,161,98>>
iex(2)> b |> MsgPack.unpack
{[1,2,["3",MsgPack.Map[map: [{"a","b"}]]]],""}
iex(3)> [b,b] |> iolist_to_binary |> MsgPack.next
{<<147,1,2,146,161,51,129,161,97,161,98>>,<<147,1,2,146,161,51,129,161,97,161,98>>}
```
