defmodule Gettext.PO.Parser do
  @moduledoc false

  alias Gettext.PO.Translations
  alias Gettext.PO.Translation
  alias Gettext.PO.PluralTranslation

  @doc """
  Parses a list of tokens into a list of translations.
  """
  @spec parse([Gettext.PO.Tokenizer.token]) ::
    {:ok, [binary], [Gettext.PO.translation]} | Gettext.PO.parse_error
  def parse(tokens) do
    case :gettext_po_parser.parse(tokens) do
      {:ok, translations} ->
        do_parse(translations)
      {:error, _reason} = error ->
        parse_error(error)
    end
  end

  defp do_parse(translations) do
    translations = Enum.map(translations, &to_struct/1)

    case check_for_duplicates(translations) do
      {:error, _line, _reason} = error ->
        error
      :ok ->
        {top_comments, headers, translations} = extract_top_comments_and_headers(translations)
        {:ok, top_comments, headers, translations}
    end
  end

  defp to_struct({:translation, translation}),
    do: struct(Translation, translation) |> extract_references() |> extract_flags()
  defp to_struct({:plural_translation, translation}),
    do: struct(PluralTranslation, translation) |> extract_references() |> extract_flags()

  defp parse_error({:error, {line, _module, reason}}) do
    {:error, line, parse_error_reason(reason)}
  end

  defp extract_references(%{__struct__: _, comments: comments} = translation) do
    {reference_comments, other_comments} = Enum.partition(comments, &match?("#:" <> _, &1))
    references =
      reference_comments
      |> Enum.reject(fn("#:" <> comm) -> String.strip(comm) == "" end)
      |> Enum.flat_map(&parse_references/1)

    %{translation | references: references, comments: other_comments}
  end

  defp parse_references("#:" <> comment) do
    comment
    |> String.strip()
    |> String.split(":")                      # we remain with "21 foo.ex"
    |> Enum.flat_map(&parse_reference_part/1) # [file, line, file, line...]
    |> Enum.chunk(2)                          # [[file, line], [file, line], ...]
    |> Enum.map(&List.to_tuple/1)             # [{file, line}, {file, line}, ...]
  end

  defp parse_reference_part(part) do
    case Integer.parse(part) do
      {next_line_no, ""}       -> [next_line_no] # last line number
      {next_line_no, filename} -> [next_line_no, String.lstrip(filename)]
      :error                   -> [part] # first filename
    end
  end

  defp extract_flags(%{__struct__: _, comments: comments} = translation) do
    {flag_comments, other_comments} = Enum.partition(comments, &match?("#," <> _, &1))
    %{translation | flags: parse_flags(flag_comments), comments: other_comments}
  end

  defp parse_flags(flag_comments) do
    flag_comments
    |> Stream.map(fn("#," <> content) -> content end)
    |> Stream.flat_map(&String.split(&1, ~r/\s+/, trim: true))
    |> Enum.into(MapSet.new)
  end

  # If the first translation has an empty msgid, it's assumed to represent
  # headers. Headers will be in the msgstr of this "fake" translation, one on
  # each line. For now, we'll just separate those lines in order to get a list
  # of headers.
  defp extract_top_comments_and_headers([%Translation{msgid: id, msgstr: headers, comments: comments} | rest])
      when id == "" or id == [""] do
    {comments, headers, rest}
  end

  defp extract_top_comments_and_headers(translations) do
    {[], [], translations}
  end

  defp check_for_duplicates(translations) do
    duplicate = Enum.reduce_while(translations, %{}, fn(t, acc) ->
      key = Translations.key(t)
      if old_line = acc[key] do
        {:halt, {t, old_line}}
      else
        {:cont, Map.put(acc, key, t.po_source_line)}
      end
    end)

    case duplicate do
      {t, old_line} ->
        build_duplicated_error(t, old_line)
      _other ->
        :ok
    end
  end

  defp build_duplicated_error(%Translation{} = t, old_line) do
    id = IO.iodata_to_binary(t.msgid)
    {:error, t.po_source_line, "found duplicate on line #{old_line} for msgid: '#{id}'"}
  end

  defp build_duplicated_error(%PluralTranslation{} = t, old_line) do
    id  = IO.iodata_to_binary(t.msgid)
    idp = IO.iodata_to_binary(t.msgid_plural)
    msg = "found duplicate on line #{old_line} for msgid: '#{id}' and msgid_plural: '#{idp}'"
    {:error, t.po_source_line, msg}
  end

  # We need to explicitly parse the error reason that yecc spits out because a
  # `{type, line, token}` token is printed as the Erlang term in the error (by
  # yecc). So, for example, if a token has a binary value then yecc will return
  # something like:
  #
  #     syntax error before: <<"my token">>
  #
  # which is not what we want, as we want the term to be printed as an Elixir
  # term. While this is ugly, it's necessary (as yecc is not very extensible)
  # and is what Elixir itself does
  # (https://github.com/elixir-lang/elixir/blob/b80651/lib/elixir/src/elixir_errors.erl#L51-L103).
  defp parse_error_reason([error, token]) do
    parse_error_reason(error, to_string(token))
    |> IO.chardata_to_string()
  end

  defp parse_error_reason('syntax error before: ' = prefix, "<<" <> rest),
    do: [prefix, binary_part(rest, 0, byte_size(rest) - 2)]
  defp parse_error_reason(error, token),
    do: [error, token]
end
