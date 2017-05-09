defmodule Plug.Conn.Utils do
  @moduledoc """
  Utilities for working with connection data
  """

  @type params :: %{binary => binary}

  @upper ?A..?Z
  @lower ?a..?z
  @alpha ?0..?9
  @other [?., ?-, ?+]
  @space [?\s, ?\t]
  @specials ~c|()<>@,;:\\"/[]?={}|

  @doc ~S"""
  Parses media types (with wildcards).

  Type and subtype are case insensitive while the
  sensitiveness of params depends on their keys and
  therefore are not handled by this parser.

  Returns:

    * `{:ok, type, subtype, map_of_params}` if everything goes fine
    * `:error` if the media type isn't valid

  ## Examples

      iex> media_type "text/plain"
      {:ok, "text", "plain", %{}}

      iex> media_type "APPLICATION/vnd.ms-data+XML"
      {:ok, "application", "vnd.ms-data+xml", %{}}

      iex> media_type "text/*; q=1.0"
      {:ok, "text", "*", %{"q" => "1.0"}}

      iex> media_type "*/*; q=1.0"
      {:ok, "*", "*", %{"q" => "1.0"}}

      iex> media_type "x y"
      :error

      iex> media_type "*/html"
      :error

      iex> media_type "/"
      :error

      iex> media_type "x/y z"
      :error

  """
  @spec media_type(binary) :: {:ok, type :: binary, subtype :: binary, params} | :error
  def media_type(binary) do
    case strip_spaces(binary) do
      "*/*" <> t -> mt_params(t, "*", "*")
      t -> mt_first(t, "")
    end
  end

  defp mt_first(<<?/, t :: binary>>, acc) when acc != "",
    do: mt_wildcard(t, acc)
  defp mt_first(<<h, t :: binary>>, acc) when h in @upper,
    do: mt_first(t, <<acc :: binary, downcase_char(h)>>)
  defp mt_first(<<h, t :: binary>>, acc) when h in @lower or h in @alpha or h == ?-,
    do: mt_first(t, <<acc :: binary, h>>)
  defp mt_first(_, _acc),
    do: :error

  defp mt_wildcard(<<?*, t :: binary>>, first),
    do: mt_params(t, first, "*")
  defp mt_wildcard(t, first),
    do: mt_second(t, "", first)

  defp mt_second(<<h, t :: binary>>, acc, first) when h in @upper,
    do: mt_second(t, <<acc :: binary, downcase_char(h)>>, first)
  defp mt_second(<<h, t :: binary>>, acc, first) when h in @lower or h in @alpha or h in @other,
    do: mt_second(t, <<acc :: binary, h>>, first)
  defp mt_second(t, acc, first),
    do: mt_params(t, first, acc)

  defp mt_params(t, first, second) do
    case strip_spaces(t) do
      ""       -> {:ok, first, second, %{}}
      ";" <> t -> {:ok, first, second, params(t)}
      _        -> :error
    end
  end

  @doc ~S"""
  Parses content type (without wildcards).

  It is similar to `media_type/1` except wildcards are
  not accepted in the type nor in the subtype.

  ## Examples

      iex> content_type "x-sample/json; charset=utf-8"
      {:ok, "x-sample", "json", %{"charset" => "utf-8"}}

      iex> content_type "x-sample/json  ; charset=utf-8  ; foo=bar"
      {:ok, "x-sample", "json", %{"charset" => "utf-8", "foo" => "bar"}}

      iex> content_type "\r\n text/plain;\r\n charset=utf-8\r\n"
      {:ok, "text", "plain", %{"charset" => "utf-8"}}

      iex> content_type "text/plain"
      {:ok, "text", "plain", %{}}

      iex> content_type "x/*"
      :error

      iex> content_type "*/*"
      :error

  """
  @spec content_type(binary) :: {:ok, type :: binary, subtype :: binary, params} | :error
  def content_type(binary) do
    case media_type(binary) do
      {:ok, _, "*", _}    -> :error
      {:ok, _, _, _} = ok -> ok
      :error              -> :error
    end
  end

  @doc ~S"""
  Parses headers parameters.

  Keys are case insensitive and downcased,
  invalid key-value pairs are discarded.

  ## Examples

      iex> params("foo=bar")
      %{"foo" => "bar"}

      iex> params("  foo=bar  ")
      %{"foo" => "bar"}

      iex> params("FOO=bar")
      %{"foo" => "bar"}

      iex> params("Foo=bar; baz=BOING")
      %{"foo" => "bar", "baz" => "BOING"}

      iex> params("foo=BAR ; wat")
      %{"foo" => "BAR"}

      iex> params("foo=\"bar\"; baz=\"boing\"")
      %{"foo" => "bar", "baz" => "boing"}

      iex> params("foo=\"bar;\"; baz=\"boing\"")
      %{"foo" => "bar;", "baz" => "boing"}

      iex> params("=")
      %{}

  """
  @spec params(binary) :: params
  def params(t) do
    t
    |> split_unquoted(";")
    |> Enum.reduce(%{}, &params/2)
  end

  defp params(param, acc) do
    case params_key(strip_spaces(param), "") do
      {k, v} -> Map.put(acc, k, v)
      false  -> acc
    end
  end

  defp params_key(<<?=, t :: binary>>, acc) when acc != "",
    do: params_value(t, acc)
  defp params_key(<<h, _ :: binary>>, _acc) when h in @specials or h in @space or h < 32 or h === 127,
    do: false
  defp params_key(<<h, t :: binary>>, acc),
    do: params_key(t, <<acc :: binary, downcase_char(h)>>)
  defp params_key(<<>>, _acc),
    do: false

  defp params_value(token, key) do
    case token(token) do
      false -> false
      value -> {key, value}
    end
  end

  @doc ~S"""
  Parses a value as defined in [RFC-1341](http://www.w3.org/Protocols/rfc1341/4_Content-Type.html).

  For convenience, trims whitespace at the end of the token.
  Returns `false` if the token is invalid.

  ## Examples

      iex> token("foo")
      "foo"

      iex> token("foo-bar")
      "foo-bar"

      iex> token("<foo>")
      false

      iex> token(~s["<foo>"])
      "<foo>"

      iex> token(~S["<f\oo>\"<b\ar>"])
      "<foo>\"<bar>"

      iex> token("foo  ")
      "foo"

      iex> token("foo bar")
      false

  """
  @spec token(binary) :: binary | false
  def token(""),
    do: false
  def token(<<?", quoted :: binary>>),
    do: quoted_token(quoted, "")
  def token(token),
    do: unquoted_token(token, "")

  defp quoted_token(<<>>, _acc),
    do: false
  defp quoted_token(<<?", t :: binary>>, acc),
    do: strip_spaces(t) == "" and acc
  defp quoted_token(<<?\\, h, t :: binary>>, acc),
    do: quoted_token(t, <<acc :: binary, h>>)
  defp quoted_token(<<h, t :: binary>>, acc),
    do: quoted_token(t, <<acc :: binary, h>>)

  defp unquoted_token(<<>>, acc),
    do: acc
  defp unquoted_token("\r\n" <> t, acc),
    do: strip_spaces(t) == "" and acc
  defp unquoted_token(<<h, t :: binary>>, acc) when h in @space,
    do: strip_spaces(t) == "" and acc
  defp unquoted_token(<<h, _ :: binary>>, _acc) when h in @specials or h < 32 or h === 127,
    do: false
  defp unquoted_token(<<h, t :: binary>>, acc),
    do: unquoted_token(t, <<acc :: binary, h>>)

  @doc """
  Parses a comma-separated list of header values.

  ## Examples

      iex> list("foo, bar")
      ["foo", "bar"]

      iex> list("foobar")
      ["foobar"]

      iex> list("")
      []

      iex> list("empties, , are,, filtered")
      ["empties", "are", "filtered"]

  """
  @spec list(binary) :: [binary]
  def list(binary) do
    for elem <- :binary.split(binary, ",", [:global]),
      (stripped = strip_spaces(elem)) != "",
      do: stripped
  end

  @doc """
  Validates the given binary is valid UTF-8.
  """
  @spec validate_utf8!(binary, module, binary) :: :ok | no_return
  def validate_utf8!(binary, exception, context)
  def validate_utf8!(<<_ :: utf8, t :: binary>>, exception, context) do
    validate_utf8!(t, exception, context)
  end

  def validate_utf8!(<<h, _ :: binary>>, exception, context) do
    raise exception,
      message: "invalid UTF-8 on #{context}, got byte #{h}"
  end

  def validate_utf8!(<<>>, _exception, _context) do
    :ok
  end

  ## Helpers

  defp strip_spaces("\r\n" <> t),
    do: strip_spaces(t)
  defp strip_spaces(<<h, t :: binary>>) when h in [?\s, ?\t],
    do: strip_spaces(t)
  defp strip_spaces(t),
    do: t

  defp downcase_char(char) when char in @upper, do: char + 32
  defp downcase_char(char), do: char

  defp split_unquoted(bin, s, groups \\ [<<>>], quoted? \\ false)
  defp split_unquoted(<<>>, _s, groups, _quoted?),
    do: groups
  defp split_unquoted(<<?", t :: binary>>, s, [g | groups], quoted?),
    do: split_unquoted(t, s, [<<g :: binary,?">> | groups], !quoted?)
  defp split_unquoted(<<h, t :: binary>>, s, groups, false) when <<h>> == s,
    do: split_unquoted(t, s, [<<>> | groups], false)
  defp split_unquoted(<<h, t :: binary>>, s, [g | groups], quoted?),
    do: split_unquoted(t, s, [<<g :: binary, h>> | groups], quoted?)
end
