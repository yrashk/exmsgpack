defmodule MsgPack.Match do

  def __macro__(binary) do
    quote do
      << unquote_splicing(binary) >>
    end
  end

  defmacro match({type, _, prefix}, opts) do
    __match__(type, prefix, opts)
  end
  defmacro match({type, _, prefix}, opts, [do: body]) do
    __match__(type, prefix, Keyword.merge(opts, do: body))
  end

  defp __match__(type, prefix, opts) do
    pattern = [(quote hygiene: false, do: rest :: binary)|Enum.reverse(prefix)] />
               Enum.reverse
    body = opts[:do]
    has_next = not nil?(opts[:next])
    has_unpack = not nil?(opts[:unpack])
    quote do
      unless unquote(has_next) do
        defp decode(:next, << unquote_splicing(pattern) >> = pattern) do
          prefix_size = byte_size(pattern) - byte_size(var!(rest))
          {value, _} = :erlang.split_binary(pattern, prefix_size)
          {value, var!(rest)}
        end
      else
        defp decode(:next, << unquote_splicing(pattern) >> = var!(pattern)) do
          unquote(opts[:next])
        end
      end
      unless unquote(has_unpack) do
        defp decode(:unpack, << unquote_splicing(pattern) >>) do
          value = unquote(body)
          {value, var!(rest)}
        end
      else
        defp decode(:unpack, << unquote_splicing(pattern) >>) do
          unquote(opts[:unpack])
        end
      end
      unless unquote(opts[:macro]) == false do
        defmacro unquote(type)(opts // []) do
          escaped_pattern = unquote(Macro.escape(pattern))
          keep_vars = unquote(opts[:requires] || [])
          pattern =
          lc pat inlist escaped_pattern do
            case pat do
              {:::, l, [name, value]} ->
                case name do
                  {atom, l1, quoted} when is_atom(atom) ->
                    if (opts[atom] == nil and not List.member?(keep_vars, atom)) do
                      {:::, l, [(quote do: _), value]}
                    else
                      if List.member?(keep_vars, atom) do
                        {:::, l, [opts[atom], value]}
                      else
                        {:::, l, [{:=, l, [name, opts[atom]]}, value]}
                      end
                    end
                  _ ->
                    {:::, l, [name, value]}
                end
              _ -> pat
            end
          end
          MsgPack.Match.__macro__(pattern)
        end
      end
    end
  end

end

defmodule MsgPack do
  @moduledoc """
  MsgPack module implements MessagePack (http://msgpack.org) encoding
  and decoding functionality (packing and unpacking)
  """

  defrecord Map, map: [] do
    @moduledoc """
    MsgPack.Map represents a MessagePack Map
    """
    record_type map: [{MsgPack.t, MsgPack.t}]

    @doc """
    Converts a Dict dictionary into a MsgPack.Map

    ## Example

        MsgPack.Map.from_dict(HashDict.new([{"a","b"}]))
    """
    @spec from_dict(Dict.t) :: t
    def from_dict(dict) do
      new(map: Dict.to_list(dict))
    end

    @doc """
    Converts a proplist into a MsgPack.Map

    ## Example

        MsgPack.Map.from_list([{"a","b"}])
    """
    @spec from_list(list) :: t
    def from_list(list) do
      new(map: list)
    end

    @doc """
    Converts MsgPack.Map into a proplist
    """
    @spec to_list(t) :: list
    def to_list(MsgPack.Map[map: map]) do
      map
    end

  end

  @typep map     :: Map.t
  @type  t       :: [t] | map | integer | float | binary
  @type  packed :: maybe_improper_list(binary | packed, binary | [])

  defexception InvalidTag, tag: nil do
    def message(MsgPack.InvalidTag[tag: tag]) do
      "Invalid tag 0x" <> integer_to_binary(tag, 16)
    end
  end

  defexception IncompletePacket, value: nil do
    def message(MsgPack.IncompletePacket[value: value]) do
      "Incomplete packet #{inspect value}"
    end
  end

  import MsgPack.Match
  match atom_nil(0xc0), do: nil
  match atom_false(0xc2), do: false
  match atom_true(0xc3), do: true
  match float32(0xca, _value :: [size(32), float, unit(1)]), do: _value
  match float64(0xcb, _value :: [size(64), float, unit(1)]), do: _value
  match uint8(0xcc, _value :: [unsigned, integer]), do: _value
  match uint16(0xcd, _value :: [size(16), big, unsigned, integer]), do: _value
  match uint32(0xce, _value :: [size(32), big, unsigned, integer]), do: _value
  match uint64(0xcf, _value :: [size(64), big, unsigned, integer]), do: _value
  match int8(0xd0, _value :: [signed, integer]), do: _value
  match int16(0xd1, _value :: [size(16), big, signed, integer]), do: _value
  match int32(0xd2, _value :: [size(32), big, signed, integer]), do: _value
  match int64(0xd3, _value :: [size(64), big, signed, integer]), do: _value
  match raw16(0xda, len :: [size(16), unsigned, integer, unit(1)],
                _value :: [size(len), binary]), requires: [:len] do
    _value
  end
  match raw32(0xdb, len :: [size(32), unsigned, integer, unit(1)],
                _value :: [size(len), binary]), requires: [:len] do
    _value
  end

  match uint7(0 :: size(1), _value :: size(7)), do: _value
  match int5(0b111 :: size(3), _value :: size(5)), do: _value - 0b100000
  match fix_raw(0b101 :: size(3), len :: size(5), _value :: [size(len), binary]),
        requires: [:len] do
    _value
  end

  match array16(0xdc, len :: [size(16), unsigned, integer, unit(1)]),
        requires: [:len],
        next: next_array(len, rest, 3, pattern),
        unpack: unpack_array(len, rest)

  match array32(0xdd, len :: [size(32), unsigned, integer, unit(1)]),
        requires: [:len],
        next: next_array(len, rest, 5, pattern),
        unpack: unpack_array(len, rest)

  match fix_array(0b1001 :: size(4), len :: size(4)),
        requires: [:len],
        next: next_array(len, rest, 1, pattern),
        unpack: unpack_array(len, rest)

  match map16(0xde, len :: [size(16), unsigned, integer, unit(1)]),
        requires: [:len],
        next: next_map(len, rest, 3, pattern),
        unpack: unpack_map(len, rest)

  match map32(0xdf, len :: [size(32), unsigned, integer, unit(1)]),
        requires: [:len],
        next: next_map(len, rest, 5, pattern),
        unpack: unpack_map(len, rest)

  match fix_map(0b1000 :: size(4), len :: size(4)),
        requires: [:len],
        next: next_map(len, rest, 1, pattern),
        unpack: unpack_map(len, rest)

  invalid_tags = [0xc1, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9,
                  0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9]

  lc tag inlist invalid_tags do
    Module.eval_quoted __MODULE__, (quote do
      import MsgPack.Match
      match invalid_tag(unquote(tag) :: [size(8), unit(1)]), macro: false do
        raise MsgPack.InvalidTag, tag: unquote(tag)
      end
    end)
  end

  defp decode(:next, pattern) do
    raise MsgPack.IncompletePacket, value: pattern
  end
  defp decode(:unpack, pattern) do
    raise MsgPack.IncompletePacket, value: pattern
  end

  defp next_array(0, rest, sz, pattern) do
    {value, _} = :erlang.split_binary(pattern, sz)
    {value, rest}
  end
  defp next_array(len, rest, prefix_size, pattern) do
    {sz, new_rest} =
    Enum.reduce(1..len, {prefix_size, rest}, fn(_i, {sz, bin}) ->
      {value, bin_rest} = next(bin)
      {sz + byte_size(value), bin_rest}
    end)
    {value, _} = :erlang.split_binary(pattern, sz)
    {value, new_rest}
  end

  defp unpack_array(0, rest) do
    {[], rest}
  end
  defp unpack_array(len, rest) do
    {acc, new_rest} =
    Enum.reduce(1..len, {[], rest}, fn(_i, {acc, bin}) ->
      {value, bin_rest} = unpack(bin)
      {[value|acc], bin_rest}
    end)
    {Enum.reverse(acc), new_rest}
  end

  defp next_map(0, rest, sz, pattern) do
    {value, _} = :erlang.split_binary(pattern, sz)
    {value, rest}
  end
  defp next_map(len, rest, prefix_size, pattern) do
    {sz, new_rest} =
    Enum.reduce(1..len, {prefix_size, rest}, fn(_i, {sz, bin}) ->
      {key, bin_rest} = next(bin)
      {value, bin_rest} = next(bin_rest)
      {sz + byte_size(key) + byte_size(value), bin_rest}
    end)
    {value, _} = :erlang.split_binary(pattern, sz)
    {value, new_rest}
  end

  defp unpack_map(0, rest) do
    {MsgPack.Map.from_list([]), rest}
  end
  defp unpack_map(len, rest) do
    {acc, new_rest} =
    Enum.reduce(1..len, {[], rest}, fn(_i, {acc, bin}) ->
      {key, bin_rest} = unpack(bin)
      {value, bin_rest} = unpack(bin_rest)
      {[{key, value}|acc], bin_rest}
    end)
    {MsgPack.Map.from_list(Enum.reverse(acc)), new_rest}
  end

  @doc """
  Packs MsgPack compatible term into an iolist-compatible structure. In order
  to convert this to a binary, use MsgPack.packed_to_binary/1

  ## Example

      MsgPack.pack([1,2,"3"])
  """
  @spec pack(t) :: packed
  def pack(term) do
    MsgPack.Protocol.pack(term)
  end

  @doc """
  Converts an iolist-compatible structure produced by MsgPack.pack/1
  into a binary
  """
  @spec packed_to_binary(packed) :: binary
  def packed_to_binary(packed) do
    iolist_to_binary(packed)
  end

  @doc """
  Unpacks a binary into an Elixir term. Returns an unpacked term
  and the remaining binary

  ## Example

      {term, bin} = MsgPack.unpack(binary)
  """
  @spec unpack(binary) :: {t, binary}
  def unpack(pattern), do: decode(:unpack, pattern)


  @doc """
  Grabs the next MsgPack term from a binary. Returns the term
  as a binary and the remaining binary. The important part
  is that this function will not allocate memory for Elixir terms,
  so if you don't need the term itself, or don't need it immediately,
  this is the function to use.

  ## Example

      {binterm, bin} = MsgPack.next(binary)
  """
  @spec next(binary) :: {binary, binary}
  def next(pattern), do: decode(:next, pattern)

end

defprotocol MsgPack.Protocol do
  @spec pack(term) :: MsgPack.packed
  def pack(term)
end

defimpl MsgPack.Protocol, for: Number do

  @spec pack(number) :: MsgPack.packed
  def pack(i) when is_integer(i) and i < 0 do
    pack_int(i)
  end
  def pack(i) when is_integer(i) do
    pack_uint(i)
  end

  # Erlang's float is always IEEE 754 64bit format
  # therefore we don't pack 0xca
  def pack(f) when is_float(f) do
    << 0xcb, f :: [size(8), big, float, unit(8)] >>
  end

  defp pack_uint(i) when i < 128 do
    << 0b0 :: size(1), i :: size(7) >>
  end
  defp pack_uint(i) when i < 256 do
    << 0xcc :: [size(8), unit(1)], i :: [size(8), unit(1)] >>
  end
  defp pack_uint(i) when i < 65536 do
    << 0xcd :: [size(8), unit(1)], i :: [size(16), unit(1), big, unsigned, integer] >>
  end
  defp pack_uint(i) when i < 0xFFFFFFFF do
    << 0xce :: [size(8), unit(1)], i :: [size(32), unit(1), big, unsigned, integer] >>
  end
  defp pack_uint(i) do
    << 0xcf :: [size(8), unit(1)], i :: [size(64), unit(1), big, unsigned, integer] >>
  end

  defp pack_int(i) when i >= -32 do
    << 0b111 :: size(3), i :: size(5) >>
  end
  defp pack_int(i) when i > -128 do
    << 0xD0, i :: [size(8), big, signed, integer, unit(1)] >>
  end
  defp pack_int(i) when i > -32768 do
    << 0xD1, i :: [size(16), big, signed, integer, unit(1)] >>
  end
  defp pack_int(i) when i > -0x80000000 do
    << 0xD2, i :: [size(32), big, signed, integer, unit(1)] >>
  end
  defp pack_int(i) do
    << 0xD3, i :: [size(64), big, signed, integer, unit(1)] >>
  end

end

defimpl MsgPack.Protocol, for: BitString do
  @spec pack(binary) :: MsgPack.packed

  def pack(binary) when byte_size(binary) < 32 do
    << 0b101 :: size(3), (byte_size(binary)) :: size(5), binary :: binary >>
  end

  def pack(binary) when byte_size(binary) <  0x10000 do
    << 0xda :: size(8), (byte_size(binary)) :: [size(16), unit(1), big, unsigned, integer], binary :: binary >>
  end

  def pack(binary) do
    << 0xdb :: size(8), (byte_size(binary)) :: [size(32), unit(1), big, unsigned, integer], binary :: binary >>
  end

end

defimpl MsgPack.Protocol, for: Atom do
  @spec pack(nil | true | false) :: MsgPack.packed

  def pack(nil), do: << 0xc0 :: size(8) >>
  def pack(true), do: << 0xc3 :: size(8) >>
  def pack(false), do: << 0xc2 :: size(8) >>
end

defimpl MsgPack.Protocol, for: List do
  @spec pack([MsgPack.t]) :: MsgPack.packed

  def pack(array) when length(array) < 16 do
    [<< 0b1001 :: size(4), (length(array)) :: size(4) >>|elements(array)]
  end

  def pack(array) when length(array) < 0x10000 do
    [<< 0xdc :: size(8), (length(array)) :: [size(16), unit(1), big, unsigned, integer] >>|elements(array)]
  end

  def pack(array) do
    [<< 0xdd :: size(8), (length(array)) :: [size(32), unit(1), big, unsigned, integer] >>|elements(array)]
  end

  defp elements(array) do
    lc element inlist array, do: MsgPack.Protocol.pack(element)
  end

end

defimpl MsgPack.Protocol, for: MsgPack.Map do
  @spec pack([MsgPack.t]) :: MsgPack.packed

  def pack(MsgPack.Map[map: map]) when length(map) < 16 do
    [<< 0b1000 :: size(4), (length(map)) :: size(4) >>|elements(map)]
  end

  def pack(MsgPack.Map[map: map]) when length(map) < 0x10000 do
    [<< 0xde :: size(8), (length(map)) :: [size(16), unit(1), big, unsigned, integer] >>|elements(map)]
  end

  def pack(MsgPack.Map[map: map]) do
    [<< 0xdf :: size(8), (length(map)) :: [size(32), unit(1), big, unsigned, integer] >>|elements(map)]
  end

  defp elements(map) do
    lc {key, value} inlist map do
      [MsgPack.Protocol.pack(key),
       MsgPack.Protocol.pack(value)]
    end
  end

end