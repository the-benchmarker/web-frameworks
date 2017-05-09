defmodule Gettext.PO do
  @moduledoc """
  This module provides facilities for working with PO (`.po`) and POT (`.pot`)
  files (mainly parsing).
  """

  alias Gettext.PO
  alias Gettext.PO.Tokenizer
  alias Gettext.PO.Parser
  alias Gettext.PO.SyntaxError
  alias Gettext.PO.Translation
  alias Gettext.PO.PluralTranslation

  @type line :: pos_integer
  @type parse_error :: {:error, line, binary}
  @type translation :: Translation.t | PluralTranslation.t

  defstruct headers: [],
            translations: [],
            file: nil,
            top_of_the_file_comments: []

  @type t :: %__MODULE__{
    top_of_the_file_comments: [binary],
    headers: [binary],
    translations: [translation],
    file: nil | Path.t,
  }

  @wrapping_column 80
  @reference_comment_length String.length("#:")
  @bom <<0xef, 0xbb, 0xbf>> # (see prune_bom/2)

  @doc """
  Parses a string into a list of translations.

  This function parses a given `str` into a list of `Gettext.PO.Translation` and
  `Gettext.PO.PluralTranslation` structs. It returns `{:ok, translations}` if
  there are no errors, otherwise `{:error, line, reason}`.

  ## Examples

      iex> {:ok, po} = Gettext.PO.parse_string \"""
      ...> msgid "foo"
      ...> msgstr "bar"
      ...> \"""
      iex> [t] = po.translations
      iex> t.msgid
      ["foo"]
      iex> t.msgstr
      ["bar"]
      iex> po.headers
      []

      iex> Gettext.PO.parse_string "foo"
      {:error, 1, "unknown keyword 'foo'"}

  """
  @spec parse_string(binary) :: {:ok, t} | parse_error
  def parse_string(str) do
    str = prune_bom(str, "nofile")

    case Tokenizer.tokenize(str) do
      {:error, _line, _reason} = error ->
        error
      {:ok, tokens} ->
        case Parser.parse(tokens) do
          {:error, _line, _reason} = error ->
            error
          {:ok, top_comments, headers, translations} ->
            {:ok, %PO{headers: headers,
                      translations: translations,
                      top_of_the_file_comments: top_comments}}
        end
    end
  end

  @doc """
  Parses a string into a list of translations, raising an exception if there are
  any errors.

  Works exactly like `parse_string/1`, but returns the list of translations
  if there are no errors or raises a `Gettext.PO.SyntaxError` error if there
  are.

  ## Examples

      iex> Gettext.PO.parse_string!("msgid")
      ** (Gettext.PO.SyntaxError) 1: no space after 'msgid'

  """
  @spec parse_string!(binary) :: t | no_return
  def parse_string!(str) do
    case parse_string(str) do
      {:ok, parsed} ->
        parsed
      {:error, line, reason} ->
        raise SyntaxError, line: line, reason: reason
    end
  end

  @doc """
  Parses the contents of a file into a list of translations.

  This function works similarly to `parse_string/1` except that it takes a file
  and parses the contents of that file. It can return:

    * `{:ok, translations}`
    * `{:error, line, reason}` if there is an error with the contents of the
      `.po` file (e.g., a syntax error)
    * `{:error, reason}` if there is an error with reading the file (this error
      is one of the errors that can be returned by `File.read/1`)_

  ## Examples

      Gettext.PO.parse_file "translations.po"
      #=> {:ok, [%Gettext.PO.Translation{msgid: "foo", msgstr: "bar"}]}

      Gettext.PO.parse_file "nonexistent"
      #=> {:error, :enoent}

  """
  @spec parse_file(Path.t) :: {:ok, t} | parse_error | {:error, atom}
  def parse_file(path) do
    case File.read(path) do
      {:ok, contents} ->
        case parse_string(prune_bom(contents, path)) do
          {:ok, po}                        -> {:ok, %{po | file: path}}
          {:error, _line, _reason} = error -> error
        end
      {:error, _reason} = error ->
        error
    end
  end

  @doc """
  Parses the contents of a file into a list of translations, raising if there
  are any errors.

  Works like `parse_file/1`, except that it raises a `Gettext.PO.SyntaxError`
  exception if there's a syntax error in the file or a `File.Error` error if
  there's an error with reading the file.

  ## Examples

      Gettext.PO.parse_file! "nonexistent.po"
      #=> ** (File.Error) could not parse file nonexistent.po: no such file or directory

  """
  @spec parse_file!(Path.t) :: t | no_return
  def parse_file!(path) do
    case parse_file(path) do
      {:ok, parsed} ->
        parsed
      {:error, reason} ->
        raise File.Error, reason: reason, action: "parse", path: path
      {:error, line, reason} ->
        raise SyntaxError, file: path, line: line, reason: reason
    end
  end

  @doc """
  Dumps a `Gettext.PO` struct as iodata.

  This function dumps a `Gettext.PO` struct (representing a PO file) as iodata,
  which can later be written to a file or converted to a string with
  `IO.iodata_to_binary/1`.

  ## Examples

  After running the following code:

      iodata = Gettext.PO.dump %Gettext.PO{
        headers: ["Last-Translator: Jane Doe"],
        translations: [
          %Gettext.PO.Translation{msgid: "foo", msgstr: "bar", comments: "# A comment"}
        ]
      }

      File.write!("/tmp/test.po", iodata)

  the `/tmp/test.po` file would look like this:

      msgid ""
      msgstr ""
      "Last-Translator: Jane Doe"

      # A comment
      msgid "foo"
      msgstr "bar"

  """
  @spec dump(t) :: iodata
  def dump(po)

  def dump(%PO{headers: hs, translations: ts, top_of_the_file_comments: cs}) do
    [dump_top_comments(cs),
     dump_headers(hs),
     (if hs != [] and ts != [], do: ?\n, else: []),
     dump_translations(ts)]
  end

  defp dump_top_comments(top_comments) when is_list(top_comments) do
    Enum.map(top_comments, &[&1, ?\n])
  end

  defp dump_headers([]) do
    []
  end

  # We do this because we want headers to be shaped like this:
  #
  #   msgid ""
  #   msgstr ""
  #   "Header: foo\n"
  defp dump_headers([first | _] = headers) when first != "" do
    dump_headers(["" | headers])
  end

  defp dump_headers(headers) do
    [~s(msgid ""\n),
      dump_kw_and_strings("msgstr", headers)]
  end

  defp dump_translations(translations) do
    translations
    |> Enum.map(&dump_translation/1)
    |> Enum.intersperse(?\n)
  end

  defp dump_translation(%Translation{} = t) do
    [dump_comments(t.comments),
     dump_flags(t.flags), dump_references(t.references),
     dump_kw_and_strings("msgid", t.msgid),
     dump_kw_and_strings("msgstr", t.msgstr)]
  end

  defp dump_translation(%PluralTranslation{} = t) do
    [dump_comments(t.comments),
     dump_flags(t.flags),
     dump_references(t.references),
     dump_kw_and_strings("msgid", t.msgid),
     dump_kw_and_strings("msgid_plural", t.msgid_plural),
     dump_plural_msgstr(t.msgstr)]
  end

  defp dump_comments(comments) do
    Enum.map(comments, &[&1, ?\n])
  end

  defp dump_references([]) do
    ""
  end

  defp dump_references(references) do
    # This function outputs a bunch of #: comments with as many references on
    # each line as there can be under @reference_wrapping_column columns.
    # In the flat_map_reduce, we insert newlines and comment delimiters (#:)
    # when references wrap, building an iolist. At the end, we need to insert
    # the first comment delimiter and the last newline.
    wrapping_column = @wrapping_column - @reference_comment_length
    {iolist, _} = Enum.flat_map_reduce references, 0, fn({file, line}, line_length) ->
      ref = " #{file}:#{line}"
      ref_length = String.length(ref)
      if (ref_length + line_length) > wrapping_column do
        {[?\n, "#:", ref], ref_length}
      else
        {[ref], line_length + ref_length}
      end
    end

    ["#:", iolist, ?\n]
  end

  defp dump_flags(flags) do
    if MapSet.size(flags) == 0 do
      ""
    else
      flags =
        flags
        |> Enum.sort
        |> Enum.map(&[?\s, &1])
      ["#,", flags, ?\n]
    end
  end

  defp dump_plural_msgstr(msgstr) do
    Enum.map msgstr, fn {plural_form, str} ->
      dump_kw_and_strings("msgstr[#{plural_form}]", str)
    end
  end

  defp dump_kw_and_strings(keyword, [first | rest]) do
    first = ~s[#{keyword} "#{escape(first)}"\n]
    rest = Enum.map(rest, &[?", escape(&1), ?", ?\n])
    [first | rest]
  end

  defp escape(str) do
    for <<char <- str>>, into: "", do: escape_char(char)
  end

  defp escape_char(?"),   do: ~S(\")
  defp escape_char(?\n),  do: ~S(\n)
  defp escape_char(?\t),  do: ~S(\t)
  defp escape_char(?\r),  do: ~S(\r)
  defp escape_char(char), do: <<char>>

  # This function removes a BOM byte sequence from the start of the given string
  # if this sequence is present. A BOM byte sequence
  # (https://en.wikipedia.org/wiki/Byte_order_mark) is a thing that Unicode uses
  # as a kind of metadata for a file; it's placed at the start of the file. GNU
  # Gettext blows up if it finds a BOM sequence at the start of a file (as you
  # can check with the `msgfmt` program); here, we don't blow up but we print a
  # warning saying the BOM is present and suggesting to remove it.
  #
  # Note that `file` is used to give a nicer warning in case the BOM is
  # present. This function is in fact called by both parse_string/1 and
  # parse_file/1. Since parse_file/1 relies on parse_string/1, in case
  # parse_file/1 is called this function is called twice but that's ok because
  # in case of BOM, parse_file/1 will remove it first and parse_string/1 won't
  # issue the warning again as its call to prune_bom/2 will be a no-op.
  defp prune_bom(str, file)

  defp prune_bom(@bom <> str, file) do
    file_or_string = if file == "nofile", do: "string", else: "file"
    IO.puts :stderr, "#{file}: warning: the #{file_or_string} being parsed starts" <>
                     " with a BOM byte sequence (#{inspect @bom, binaries: :as_binaries})." <>
                     " These bytes are ignored by Gettext but it's recommended to remove" <>
                     " them. To know more about BOM, read" <>
                     " https://en.wikipedia.org/wiki/Byte_order_mark."
    str
  end

  defp prune_bom(str, _) when is_binary(str) do
    str
  end
end
