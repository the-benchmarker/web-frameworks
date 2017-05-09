defmodule Poison.EncodeError do
  defexception value: nil, message: nil

  def message(%{value: value, message: nil}) do
    "unable to encode value: #{inspect value}"
  end

  def message(%{message: message}) do
    message
  end
end

defmodule Poison.Encode do
  defmacro __using__(_) do
    quote do
      defp encode_name(value) do
        cond do
          is_binary(value) ->
            value
          is_atom(value) ->
            Atom.to_string(value)
          true ->
            raise Poison.EncodeError, value: value,
              message: "expected string or atom key, got: #{inspect value}"
        end
      end
    end
  end
end

defmodule Poison.Pretty do
  defmacro __using__(_) do
    quote do
      @default_indent 2
      @default_offset 0

      @compile {:inline, pretty: 1, indent: 1, offset: 1, offset: 2, spaces: 1}

      defp pretty(options) do
        !!Keyword.get(options, :pretty)
      end

      defp indent(options) do
        Keyword.get(options, :indent, @default_indent)
      end

      defp offset(options) do
        Keyword.get(options, :offset, @default_offset)
      end

      defp offset(options, value) do
        Keyword.put(options, :offset, value)
      end

      defp spaces(count) do
        :binary.copy(" ", count)
      end
    end
  end
end

defprotocol Poison.Encoder do
  @fallback_to_any true

  def encode(value, options)
end

defimpl Poison.Encoder, for: Atom do
  def encode(nil, _),   do: "null"
  def encode(true, _),  do: "true"
  def encode(false, _), do: "false"

  def encode(atom, options) do
    Poison.Encoder.BitString.encode(Atom.to_string(atom), options)
  end
end

defimpl Poison.Encoder, for: BitString do
  use Bitwise

  def encode("", _), do: "\"\""

  def encode(string, options) do
    [?", escape(string, options[:escape]), ?"]
  end

  defp escape("", _), do: []

  for {char, seq} <- Enum.zip('"\\\n\t\r\f\b', '"\\ntrfb') do
    defp escape(<<unquote(char)>> <> rest, mode) do
      [unquote("\\" <> <<seq>>) | escape(rest, mode)]
    end
  end

  # http://en.wikipedia.org/wiki/Unicode_control_characters
  defp escape(<<char>> <> rest, mode) when char <= 0x1F or char == 0x7F do
    [seq(char) | escape(rest, mode)]
  end

  defp escape(<<char :: utf8>> <> rest, mode) when char in 0x80..0x9F do
    [seq(char) | escape(rest, mode)]
  end

  defp escape(<<char :: utf8>> <> rest, :unicode) when char in 0xA0..0xFFFF do
    [seq(char) | escape(rest, :unicode)]
  end

  # http://en.wikipedia.org/wiki/UTF-16#Example_UTF-16_encoding_procedure
  # http://unicodebook.readthedocs.org/unicode_encodings.html#utf-16-surrogate-pairs
  defp escape(<<char :: utf8>> <> rest, :unicode) when char > 0xFFFF do
    code = char - 0x10000
    [seq(0xD800 ||| (code >>> 10)),
     seq(0xDC00 ||| (code &&& 0x3FF))
     | escape(rest, :unicode)]
  end

  defp escape(<<char :: utf8>> <> rest, :javascript) when char in [0x2028, 0x2029] do
    [seq(char) | escape(rest, :javascript)]
  end

  defp escape(string, mode) do
    size = chunk_size(string, mode, 0)
    <<chunk :: binary-size(size), rest :: binary>> = string
    [chunk | escape(rest, mode)]
  end

  defp chunk_size(<<char>> <> _, _mode, acc) when char <= 0x1F or char in '"\\' do
    acc
  end

  defp chunk_size(<<char>> <> rest, mode, acc) when char < 0x80 do
    chunk_size(rest, mode, acc + 1)
  end

  defp chunk_size(<<_ :: utf8>> <> _, :unicode, acc) do
    acc
  end

  defp chunk_size(<<char :: utf8>> <> _, :javascript, acc) when char in [0x2028, 0x2029] do
    acc
  end

  defp chunk_size(<<codepoint :: utf8>> <> rest, mode, acc) do
    size = cond do
      codepoint < 0x800   -> 2
      codepoint < 0x10000 -> 3
      true                -> 4
    end

    chunk_size(rest, mode, acc + size)
  end

  defp chunk_size(<<char>>, _, _) do
    raise Poison.EncodeError, value: <<char>>
  end

  defp chunk_size("", _, acc), do: acc

  @compile {:inline, seq: 1}
  defp seq(char) do
    case Integer.to_char_list(char, 16) do
      s when length(s) < 2 -> ["\\u000" | s]
      s when length(s) < 3 -> ["\\u00" | s]
      s when length(s) < 4 -> ["\\u0" | s]
      s -> ["\\u" | s]
    end
  end
end

defimpl Poison.Encoder, for: Integer do
  def encode(integer, _options) do
    Integer.to_string(integer)
  end
end

defimpl Poison.Encoder, for: Float do
  def encode(float, _options) do
    :io_lib_format.fwrite_g(float)
  end
end

defimpl Poison.Encoder, for: Map do
  alias Poison.Encoder

  @compile :inline_list_funcs

  use Poison.Pretty
  use Poison.Encode

  # TODO: Remove once we require Elixir 1.1+
  defmacro __deriving__(module, struct, options) do
    Poison.Encoder.Any.deriving(module, struct, options)
  end

  def encode(map, _) when map_size(map) < 1, do: "{}"

  def encode(map, options) do
    encode(map, pretty(options), options)
  end

  def encode(map, true, options) do
    indent = indent(options)
    offset = offset(options) + indent
    options = offset(options, offset)

    fun = &[",\n", spaces(offset), Encoder.BitString.encode(encode_name(&1), options), ": ",
                                   Encoder.encode(:maps.get(&1, map), options) | &2]
    ["{\n", tl(:lists.foldl(fun, [], :maps.keys(map))), ?\n, spaces(offset - indent), ?}]
  end

  def encode(map, _, options) do
    fun = &[?,, Encoder.BitString.encode(encode_name(&1), options), ?:,
                Encoder.encode(:maps.get(&1, map), options) | &2]
    [?{, tl(:lists.foldl(fun, [], :maps.keys(map))), ?}]
  end
end

defimpl Poison.Encoder, for: List do
  alias Poison.Encoder

  use Poison.Pretty

  @compile :inline_list_funcs

  def encode([], _), do: "[]"

  def encode(list, options) do
    encode(list, pretty(options), options)
  end

  def encode(list, false, options) do
    fun = &[?,, Encoder.encode(&1, options) | &2]
    [?[, tl(:lists.foldr(fun, [], list)), ?]]
  end

  def encode(list, true, options) do
    indent = indent(options)
    offset = offset(options) + indent
    options = offset(options, offset)

    fun = &[",\n", spaces(offset), Encoder.encode(&1, options) | &2]
    ["[\n", tl(:lists.foldr(fun, [], list)), ?\n, spaces(offset - indent), ?]]
  end
end

defimpl Poison.Encoder, for: [Range, Stream, MapSet, HashSet] do
  use Poison.Pretty

  def encode(collection, options) do
    encode(collection, pretty(options), options)
  end

  def encode(collection, false, options) do
    fun = &[?,, Poison.Encoder.encode(&1, options)]

    case Enum.flat_map(collection, fun) do
      [] -> "[]"
      [_ | tail] -> [?[, tail, ?]]
    end
  end

  def encode(collection, true, options) do
    indent = indent(options)
    offset = offset(options) + indent
    options = offset(options, offset)

    fun = &[",\n", spaces(offset), Poison.Encoder.encode(&1, options)]

    case Enum.flat_map(collection, fun) do
      [] -> "[]"
      [_ | tail] -> ["[\n", tail, ?\n, spaces(offset - indent), ?]]
    end
  end
end

defimpl Poison.Encoder, for: HashDict do
  alias Poison.Encoder

  use Poison.Pretty
  use Poison.Encode

  def encode(dict, options) do
    if HashDict.size(dict) < 1 do
      "{}"
    else
      encode(dict, pretty(options), options)
    end
  end

  def encode(dict, false, options) do
    fun = fn {key, value} ->
      [?,, Encoder.BitString.encode(encode_name(key), options), ?:,
           Encoder.encode(value, options)]
    end

    [?{, tl(Enum.flat_map(dict, fun)), ?}]
  end

  def encode(dict, true, options) do
    indent = indent(options)
    offset = offset(options) + indent
    options = offset(options, offset)

    fun = fn {key, value} ->
      [",\n", spaces(offset), Encoder.BitString.encode(encode_name(key), options), ": ",
                              Encoder.encode(value, options)]
    end

    ["{\n", tl(Enum.flat_map(dict, fun)), ?\n, spaces(offset - indent), ?}]
  end
end

if Version.match?(System.version, ">=1.3.0-rc.1") do
  defimpl Poison.Encoder, for: [Date, Time, NaiveDateTime, DateTime] do
    def encode(value, options) do
      Poison.Encoder.BitString.encode(@for.to_iso8601(value), options)
    end
  end
end

defimpl Poison.Encoder, for: Any do
  defmacro __deriving__(module, struct, options) do
    deriving(module, struct, options)
  end

  def deriving(module, _struct, options) do
    only = options[:only]
    except = options[:except]

    extractor = cond do
      only ->
        quote(do: Map.take(struct, unquote(only)))
      except ->
        except = [:__struct__ | except]
        quote(do: Map.drop(struct, unquote(except)))
      true ->
        quote(do: :maps.remove(:__struct__, struct))
    end

    quote do
      defimpl Poison.Encoder, for: unquote(module) do
        def encode(struct, options) do
          Poison.Encoder.Map.encode(unquote(extractor), options)
        end
      end
    end
  end

  def encode(%{__struct__: _} = struct, options) do
    Poison.Encoder.Map.encode(Map.from_struct(struct), options)
  end

  def encode(value, _options) do
    raise Poison.EncodeError, value: value
  end
end
