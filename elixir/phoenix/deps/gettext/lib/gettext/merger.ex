defmodule Gettext.Merger do
  @moduledoc false

  alias Gettext.PO
  alias Gettext.PO.Translation
  alias Gettext.PO.PluralTranslation
  alias Gettext.Fuzzy

  @new_po_informative_comment """
  ## `msgid`s in this file come from POT (.pot) files.
  ##
  ## Do not add, change, or remove `msgid`s manually here as
  ## they're tied to the ones in the corresponding POT file
  ## (with the same domain).
  ##
  ## Use `mix gettext.extract --merge` or `mix gettext.merge`
  ## to merge POT files into PO files.
  """

  @doc """
  Merges a PO file with a POT file given their paths.

  This function returns the contents (as iodata) of the merged file, which will
  be written to a PO file.
  """
  @spec merge_files(Path.t, Path.t, Keyword.t) :: iodata
  def merge_files(po_file, pot_file, opts) do
    merge(PO.parse_file!(po_file), PO.parse_file!(pot_file), opts) |> PO.dump
  end

  @doc """
  Merges two `Gettext.PO` structs representing a PO file and an updated POT (or
  PO) file into a new `Gettext.PO` struct.

  `old` is an existing PO file (that contains translations) which will be
  "updated" with the translations in the `new` POT or PO file. Translations in
  `old` will kept as long as they match with translations in `new`; all other
  translations will be discarded (as `new` is considered to be the reference).

  The `Gettext.PO` struct that this function returns is *alway* meant to be a PO
  file, not a POT file.

  `new` can be:

    * a POT file (usually created or updated by the `mix gettext.extract` task) or
    * a newly created PO file with up-to-date source references (but old translations)

  Note that all translator comments in `new` will be discarded in favour of the
  ones in `old`. Reference comments and extracted comments will be taken from
  `new` instead.

  The following rules are observed:

    * matching translations are merged as follows:
      * existing msgstr are preserved (the ones in the POT file are empty anyways)
      * existing translator comments are preserved (there are no translator
        comments in POT files)
      * existing references are discarded (as they're now outdated) and replaced
        by the references in the POT file

  """
  @spec merge(PO.t, PO.t, Keyword.t) :: PO.t
  def merge(%PO{} = old, %PO{} = new, opts) do
    %PO{
      top_of_the_file_comments: old.top_of_the_file_comments,
      headers: old.headers,
      file: old.file,
      translations: merge_translations(old.translations, new.translations, opts),
    }
  end

  defp merge_translations(old, new, opts) do
    # First, we convert the list of old translations into a dict for
    # constant-time lookup.
    old = for t <- old, into: %{}, do: {PO.Translations.key(t), t}

    # Then, we do a first pass through the list of new translation and we mark
    # all exact matches as {key, translation, exact_match}, taking the exact matches
    # out of `old` at the same time.
    {new, old} = Enum.map_reduce new, old, fn(t, old) ->
      key = PO.Translations.key(t)
      {same, old} = Map.pop(old, key)
      {{key, t, same}, old}
    end

    # Now, tuples like {key, translation, nil} identify translations with no
    # exact match. For those translations, we look for a fuzzy match. We ditch
    # the obsolete translations altogether.
    {new, _obsolete} = Enum.map_reduce new, old, fn
      {key, t, nil}, old ->
        if Keyword.fetch!(opts, :fuzzy) do
          find_fuzzy_match(key, t, old, Keyword.fetch!(opts, :fuzzy_threshold))
        else
          {t, old}
        end
      {_, t, exact_match}, old ->
        {merge_two_translations(exact_match, t), old}
    end

    new
  end

  # Returns {fuzzy_matched_translation, updated_old_translations} if a match is
  # found, otherwise {original_translation, old_translations}.
  defp find_fuzzy_match(key, target, old_translations, threshold) do
    matcher = Fuzzy.matcher(threshold)

    candidates =
      old_translations
      |> Enum.map(fn {k, t} -> {k, t, matcher.(k, key)} end)
      |> Enum.reject(&match?({_, _, :nomatch}, &1))

    if candidates == [] do
      {target, old_translations}
    else
      {k, t, _} = Enum.max_by(candidates, fn {_, _, {:match, distance}} -> distance end)
      {Fuzzy.merge(target, t), Map.delete(old_translations, k)}
    end
  end

  defp merge_two_translations(%Translation{} = old, %Translation{} = new) do
    %Translation{
      msgid: new.msgid, # they are the same
      msgstr: old.msgstr, # new.msgstr should be empty since it's a POT file
      comments: old.comments, # new has no translator comments
      references: new.references,
    }
  end

  defp merge_two_translations(%PluralTranslation{} = old, %PluralTranslation{} = new) do
    %PluralTranslation{
      msgid: new.msgid, # they are the same
      msgid_plural: new.msgid_plural, # they are the same
      msgstr: old.msgstr, # new.msgstr should be empty since it's a POT file
      comments: old.comments, # new has no translator comments
      references: new.references,
    }
  end

  @doc """
  Returns the contents of a new PO file to be written at `po_file` from the POT
  template in `pot_file`.

  The new PO file will have:

    * the `Language` header set based on the locale (extracted from the path)
    * the translations of the POT file (no merging is needed as there are no
      translations in the PO file)

  Comments in `pot_file` that start with `##` will be discarded and not copied
  over the new PO file as they're meant to be comments generated by tools or
  comments directed to developers.
  """
  @spec new_po_file(Path.t, Path.t) :: iodata
  def new_po_file(po_file, pot_file) do
    pot = PO.parse_file!(pot_file)
    po = %PO{
      headers: headers_for_new_po_file(po_file),
      file: po_file,
      translations: strip_double_hash_comments(pot.translations),
    }

    [@new_po_informative_comment, PO.dump(po)]
  end

  defp headers_for_new_po_file(po_file) do
    [
      ~s(Language: #{locale_from_path(po_file)}\n),
    ]
  end

  defp locale_from_path(path) do
    parts = Path.split(path)
    index = Enum.find_index(parts, &(&1 == "LC_MESSAGES"))
    Enum.at(parts, index - 1)
  end

  defp strip_double_hash_comments(translations) when is_list(translations) do
    for %{comments: comments} = t <- translations do
      %{t | comments: Enum.reject(comments, &match?("##" <> _, &1))}
    end
  end
end
