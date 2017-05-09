defmodule Poison.SyntaxError do
  defexception [:message, :token]

  def exception(opts) do
    message = if token = opts[:token] do
      "Unexpected token: #{token}"
    else
      "Unexpected end of input"
    end

    %Poison.SyntaxError{message: message, token: token}
  end
end

defmodule Poison.Parser do
  @moduledoc """
  An ECMA 404 conforming JSON parser.

  See: http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
  """

  if Application.get_env(:poison, :native) do
    @compile :native
  end

  use Bitwise

  alias Poison.SyntaxError

  @type t :: nil | true | false | list | float | integer | String.t | map

  @spec parse(iodata, Keyword.t) :: {:ok, t} | {:error, :invalid}
    | {:error, {:invalid, String.t}}
  def parse(iodata, options \\ []) do
    string = IO.iodata_to_binary(iodata)
    {value, rest} = value(skip_whitespace(string), options[:keys])
    case skip_whitespace(rest) do
      "" -> {:ok, value}
      other -> syntax_error(other)
    end
  catch
    :invalid ->
      {:error, :invalid}
    {:invalid, token} ->
      {:error, {:invalid, token}}
  end

  @spec parse!(iodata, Keyword.t) :: t
  def parse!(iodata, options \\ []) do
    case parse(iodata, options) do
      {:ok, value} ->
        value
      {:error, :invalid} ->
        raise SyntaxError
      {:error, {:invalid, token}} ->
        raise SyntaxError, token: token
    end
  end

  defp value("\"" <> rest, _keys),    do: string_continue(rest, [])
  defp value("{" <> rest, keys),      do: object_pairs(skip_whitespace(rest), keys, [])
  defp value("[" <> rest, keys),      do: array_values(skip_whitespace(rest), keys, [])
  defp value("null" <> rest, _keys),  do: {nil, rest}
  defp value("true" <> rest, _keys),  do: {true, rest}
  defp value("false" <> rest, _keys), do: {false, rest}

  defp value(<<char, _ :: binary>> = string, _keys) when char in '-0123456789' do
    number_start(string)
  end

  defp value(other, _keys), do: syntax_error(other)

  ## Objects

  defp object_pairs("\"" <> rest, keys, acc) do
    {name, rest} = string_continue(rest, [])
    {value, rest} = case skip_whitespace(rest) do
      ":" <> rest -> value(skip_whitespace(rest), keys)
      other -> syntax_error(other)
    end

    acc = [{object_name(name, keys), value} | acc]
    case skip_whitespace(rest) do
      "," <> rest -> object_pairs(skip_whitespace(rest), keys, acc)
      "}" <> rest -> {:maps.from_list(acc), rest}
      other -> syntax_error(other)
    end
  end

  defp object_pairs("}" <> rest, _, []) do
    {:maps.new, rest}
  end

  defp object_pairs(other, _, _), do: syntax_error(other)

  defp object_name(name, :atoms),  do: String.to_atom(name)
  defp object_name(name, :atoms!), do: String.to_existing_atom(name)
  defp object_name(name, _keys),   do: name

  ## Arrays

  defp array_values("]" <> rest, _, []) do
    {[], rest}
  end

  defp array_values(string, keys, acc) do
    {value, rest} = value(string, keys)

    acc = [value | acc]
    case skip_whitespace(rest) do
      "," <> rest -> array_values(skip_whitespace(rest), keys, acc)
      "]" <> rest -> {:lists.reverse(acc), rest}
      other -> syntax_error(other)
    end
  end

  ## Numbers

  defp number_start("-" <> rest) do
    case rest do
      "0" <> rest -> number_frac(rest, ["-0"])
      rest -> number_int(rest, [?-])
    end
  end

  defp number_start("0" <> rest) do
    number_frac(rest, [?0])
  end

  defp number_start(string) do
    number_int(string, [])
  end

  defp number_int(<<char, _ :: binary>> = string, acc) when char in '123456789' do
    {digits, rest} = number_digits(string)
    number_frac(rest, [acc, digits])
  end

  defp number_int(other, _), do: syntax_error(other)

  defp number_frac("." <> rest, acc) do
    {digits, rest} = number_digits(rest)
    number_exp(rest, true, [acc, ?., digits])
  end

  defp number_frac(string, acc) do
    number_exp(string, false, acc)
  end

  defp number_exp(<<e>> <> rest, frac, acc) when e in 'eE' do
    e = if frac, do: ?e, else: ".0e"
    case rest do
      "-" <> rest -> number_exp_continue(rest, [acc, e, ?-])
      "+" <> rest -> number_exp_continue(rest, [acc, e])
      rest -> number_exp_continue(rest, [acc, e])
    end
  end

  defp number_exp(string, frac, acc) do
    {number_complete(acc, frac), string}
  end

  defp number_exp_continue(rest, acc) do
    {digits, rest} = number_digits(rest)
    {number_complete([acc, digits], true), rest}
  end

  defp number_complete(iolist, false) do
    IO.iodata_to_binary(iolist) |> String.to_integer
  end

  defp number_complete(iolist, true) do
    IO.iodata_to_binary(iolist) |> String.to_float
  end

  defp number_digits(<<char>> <> rest = string) when char in '0123456789' do
    count = number_digits_count(rest, 1)
    <<digits :: binary-size(count), rest :: binary>> = string
    {digits, rest}
  end

  defp number_digits(other), do: syntax_error(other)

  defp number_digits_count(<<char>> <> rest, acc) when char in '0123456789' do
    number_digits_count(rest, acc + 1)
  end

  defp number_digits_count(_, acc), do: acc

  ## Strings

  defp string_continue("\"" <> rest, acc) do
    {IO.iodata_to_binary(acc), rest}
  end

  defp string_continue("\\" <> rest, acc) do
    string_escape(rest, acc)
  end

  defp string_continue("", _), do: throw(:invalid)

  defp string_continue(string, acc) do
    n = string_chunk_size(string, 0)
    <<chunk :: binary-size(n), rest :: binary>> = string
    string_continue(rest, [acc, chunk])
  end

  for {seq, char} <- Enum.zip('"\\ntr/fb', '"\\\n\t\r/\f\b') do
    defp string_escape(<<unquote(seq)>> <> rest, acc) do
      string_continue(rest, [acc, unquote(char)])
    end
  end

  # http://www.ietf.org/rfc/rfc2781.txt
  # http://perldoc.perl.org/Encode/Unicode.html#Surrogate-Pairs
  # http://mathiasbynens.be/notes/javascript-encoding#surrogate-pairs
  defp string_escape(<<?u, a1, b1, c1, d1, "\\u", a2, b2, c2, d2>> <> rest, acc)
    when a1 in 'dD' and a2 in 'dD'
    and (b1 in '89abAB')
    and (b2 in ?c..?f or b2 in ?C..?F) \
  do
    hi = List.to_integer([a1, b1, c1, d1], 16)
    lo = List.to_integer([a2, b2, c2, d2], 16)
    codepoint = 0x10000 + ((hi &&& 0x03FF) <<< 10) + (lo &&& 0x03FF)
    string_continue(rest, [acc, <<codepoint :: utf8>>])
  end

  defp string_escape(<<?u, seq :: binary-size(4)>> <> rest, acc) do
    string_continue(rest, [acc, <<String.to_integer(seq, 16) :: utf8>> ])
  end

  defp string_escape(other, _), do: syntax_error(other)

  defp string_chunk_size("\"" <> _, acc), do: acc
  defp string_chunk_size("\\" <> _, acc), do: acc

  defp string_chunk_size(<<char>> <> rest, acc) when char < 0x80 do
    string_chunk_size(rest, acc + 1)
  end

  defp string_chunk_size(<<codepoint :: utf8>> <> rest, acc) do
    string_chunk_size(rest, acc + string_codepoint_size(codepoint))
  end

  defp string_chunk_size(other, _), do: syntax_error(other)

  defp string_codepoint_size(codepoint) when codepoint < 0x800,   do: 2
  defp string_codepoint_size(codepoint) when codepoint < 0x10000, do: 3
  defp string_codepoint_size(_),                                  do: 4

  ## Whitespace

  defp skip_whitespace(<<char>> <> rest) when char in '\s\n\t\r' do
    skip_whitespace(rest)
  end

  defp skip_whitespace(string), do: string

  ## Errors

  defp syntax_error(<<token :: utf8>> <> _) do
    throw({:invalid, <<token>>})
  end

  defp syntax_error(_) do
    throw(:invalid)
  end
end
