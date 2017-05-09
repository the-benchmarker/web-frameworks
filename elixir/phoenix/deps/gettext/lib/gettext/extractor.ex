defmodule Gettext.Extractor do
  @moduledoc false

  # This module is responsible for extracting translations (it's called from the
  # *gettext macros) and dumping those translations to POT files, merging with
  # existing POT files if necessary.
  #
  # ## Ordering
  #
  # Ordering is mostly taken care of in merge_template/2, where we go over the
  # translations in an existing POT file and merge them if necessary (thus
  # keeping the order from the original file), then adding the translations form
  # the new in-memory POT (sorted by name).

  alias Gettext.ExtractorAgent
  alias Gettext.PO
  alias Gettext.PO.Translation
  alias Gettext.PO.PluralTranslation
  alias Gettext.Error

  @doc """
  Enables translation extraction.
  """
  @spec enable() :: :ok
  def enable do
    ExtractorAgent.enable
  end

  @doc """
  Disables extraction.
  """
  @spec disable() :: :ok
  def disable do
    ExtractorAgent.disable
  end

  @doc """
  Tells whether translations are being extracted.
  """
  @spec extracting?() :: boolean
  def extracting? do
    # Because the extractor agent may not be enabled during compilation
    # time (as it requires the optional gettext compiler), we need to
    # check if the agent is up and running before querying it.
    Process.whereis(ExtractorAgent) && ExtractorAgent.extracting?
  end

  @doc """
  Extracts a translation by temporarily storing it in an agent.

  Note that this function doesn't perform any operation on the filesystem.
  """
  @spec extract(Macro.Env.t, module, binary, binary | {binary, binary}) :: :ok
  def extract(%Macro.Env{file: file, line: line} = _caller, backend, domain, id) do
    ExtractorAgent.add_translation(backend, domain, create_translation_struct(id, file, line))
  end

  @doc """
  Returns a list of POT files based o the results of the extraction.

  Returns a list of paths and their contents to be written to disk. Existing POT
  files are either purged from obsolete translations (in case no extracted
  translation ends up in that file) or merged with the extracted translations;
  new POT files are returned for extracted translations that belong to a POT
  file that doesn't exist yet.

  This is a stateful operation. Once pot_files are generated, their information
  is permanently removed from the extractor.
  """
  @spec pot_files(app :: atom, config :: Keyword.t) :: [{path :: String.t, contents :: iodata}]
  def pot_files(app, gettext_config) do
    backends = ExtractorAgent.pop_backends(app)
    existing_pot_files = pot_files_for_backends(backends)

    backends
    |> ExtractorAgent.pop_translations()
    |> create_po_structs_from_extracted_translations()
    |> merge_pot_files(existing_pot_files, gettext_config)
  end

  # Returns all the .pot files for each of the given `backends`.
  defp pot_files_for_backends(backends) do
    Enum.flat_map backends, fn backend ->
      backend.__gettext__(:priv)
      |> Path.join("**/*.pot")
      |> Path.wildcard()
    end
  end

  # This returns a list of {absolute_path, %Gettext.PO{}} tuples.
  # `all_translations` looks like this:
  #
  #     %{MyBackend => %{"a_domain" => %{"a translation id" => a_translation}}}
  #
  defp create_po_structs_from_extracted_translations(all_translations) do
    for {backend, domains}     <- all_translations,
        {domain, translations} <- domains do
      create_po_struct(backend, domain, Map.values(translations))
    end
  end

  # Returns a {path, %Gettext.PO{}} tuple.
  defp create_po_struct(backend, domain, translations) do
    {pot_path(backend, domain), po_struct_from_translations(translations)}
  end

  defp pot_path(backend, domain) do
    Path.join(backend.__gettext__(:priv), "#{domain}.pot")
  end

  defp po_struct_from_translations(translations) do
    # Sort all the translations and the references of each translation in order
    # to make as few changes as possible to the PO(T) files.
    translations =
      translations
      |> Enum.sort_by(&PO.Translations.key/1)
      |> Enum.map(&sort_references/1)

    %PO{translations: translations}
  end

  defp sort_references(translation) do
    update_in(translation.references, &Enum.sort/1)
  end

  defp create_translation_struct({msgid, msgid_plural}, file, line),
    do: %PluralTranslation{
          msgid: [msgid],
          msgid_plural: [msgid_plural],
          msgstr: %{0 => [""], 1 => [""]},
          references: [{Path.relative_to_cwd(file), line}],
        }
  defp create_translation_struct(msgid, file, line),
    do: %Translation{
          msgid: [msgid],
          msgstr: [""],
          references: [{Path.relative_to_cwd(file), line}],
        }

  # Made public for testing.
  @doc false
  def merge_pot_files(po_structs, pot_files, gettext_config) do
    # pot_files is a list of paths to existing .pot files while po_structs is a
    # list of {path, struct} for new %Gettext.PO{} structs that we have
    # extracted. If we turn pot_files into a list of {path, whatever} tuples,
    # then we can take advantage of Dict.merge/3 to find files that we have to
    # update, delete, or add.
    pot_files  = Enum.into(pot_files, %{}, &{&1, :existing})
    po_structs = Enum.into(po_structs, %{})

    # After Map.merge/3, we have something like:
    #   %{path => {:merged, :unchanged | %PO{}}, path => %PO{}, path => :existing}
    # and after mapping tag_files/1 over that we have something like:
    #   %{path => {:merged, :unchanged | %PO{}}, path => {:unmerged, :unchanged | %PO{}}, path => {:new, %PO{}}}
    Map.merge(pot_files, po_structs, &merge_existing_and_extracted(&1, &2, &3, gettext_config))
    |> Enum.map(&tag_files(&1, gettext_config))
    |> Enum.reject(&match?({_, {_, :unchanged}}, &1))
    |> Enum.map(&dump_tagged_file/1)
  end

  # This function is called by merge_pot_files/2 as the function passed to
  # Map.merge/3 (so when we have both an :existing file and a new extracted
  # in-memory PO struct both located at "path").
  defp merge_existing_and_extracted(path, :existing, extracted, gettext_config) do
    {:merged, merge_or_unchanged(path, extracted, gettext_config)}
  end

  # Returns :unchanged if merging `existing_path` with `new_po` changes nothing,
  # otherwise a %Gettext.PO{} struct with the changed contents.
  defp merge_or_unchanged(existing_path, new_po, gettext_config) do
    {existing_contents, existing_po} = read_contents_and_parse(existing_path)
    merged_po = merge_template(existing_po, new_po, gettext_config)

    if IO.iodata_to_binary(PO.dump(merged_po)) == existing_contents do
      :unchanged
    else
      merged_po
    end
  end

  defp read_contents_and_parse(path) do
    contents = File.read!(path)

    # We use `parse_string/1` and raise manually instead of using
    # `parse_string!/1` on `contents` as we want the file/line to appear in the
    # error message.
    case PO.parse_string(contents) do
      {:ok, po} ->
        {contents, po}
      {:error, line, reason} ->
        raise PO.SyntaxError, line: line, file: path, reason: reason
    end
  end

  # This function "tags" a {path, _} tuple in order to distinguish POT files
  # that have been merged (one existed at `path` and there's a new one to put at
  # `path` as well), POT files that exist but have no new counterpart (`{path,
  # :existing}`) and new files that do not exist yet.
  # These are marked as:
  #   * {path, {:merged, _}} - one existed and there's a new one
  #   * {path, {:unmerged, _}} - one existed, no new one
  #   * {path, {:new, _}} - none existed, there's a new one
  # Note that existing files with no new corresponding file are "pruned", e.g.,
  # merged with an empty %PO{} struct to remove obsolete translations (see
  # prune_unmerged/1), because the user could still have PO translation that
  # they manually inserted in that file.
  defp tag_files({_path, {:merged, _}} = entry, _gettext_config),
    do: entry
  defp tag_files({path, :existing}, gettext_config),
    do: {path, {:unmerged, prune_unmerged(path, gettext_config)}}
  defp tag_files({path, new_po}, _gettext_config),
    do: {path, {:new, new_po}}

  # This function "dumps" merged files and unmerged files without any changes,
  # and dumps new POT files adding an informative comment to them. This doesn't
  # write anything to disk, it just returns `{path, contents}` tuples.
  defp dump_tagged_file({path, {:new, new_po}}),
    do: {path, [new_pot_comment(), (new_po |> add_headers_to_new_po() |> PO.dump())]}
  defp dump_tagged_file({path, {tag, po}}) when tag in [:unmerged, :merged],
    do: {path, PO.dump(po)}

  defp prune_unmerged(path, gettext_config) do
    merge_or_unchanged(path, %PO{}, gettext_config)
  end

  defp new_pot_comment do
    """
    ## This file is a PO Template file.
    ##
    ## `msgid`s here are often extracted from source code.
    ## Add new translations manually only if they're dynamic
    ## translations that can't be statically extracted.
    ##
    ## Run `mix gettext.extract` to bring this file up to
    ## date. Leave `msgstr`s empty as changing them here as no
    ## effect: edit them in PO (`.po`) files instead.
    """
  end

  defp add_headers_to_new_po(%PO{headers: []} = po) do
    %{po | headers: [""]}
  end

  # Merges a %PO{} struct representing an existing POT file with an
  # in-memory-only %PO{} struct representing the new POT file.
  # Made public for testing.
  @doc false
  def merge_template(existing, new, gettext_config) do
    protected_pattern = gettext_config[:excluded_refs_from_purging]

    # We go over the existing translations in order so as to keep the existing
    # order as much as possible.
    old_and_merged = Enum.flat_map existing.translations, fn(t) ->
      cond do
        same = PO.Translations.find(new.translations, t) ->
          [merge_translations(t, same)]
        PO.Translations.protected?(t, protected_pattern) ->
          [t]
        PO.Translations.autogenerated?(t) ->
          []
        true ->
          [t]
      end
    end

    # We reject all translations that appear in `existing` so that we're left
    # with the translations that only appear in `new`.
    unique_new = Enum.reject(new.translations, &PO.Translations.find(existing.translations, &1))

    %PO{translations: old_and_merged ++ unique_new,
        headers: existing.headers,
        top_of_the_file_comments: existing.top_of_the_file_comments}
  end

  defp merge_translations(%Translation{} = old, %Translation{comments: []} = new) do
    ensure_empty_msgstr!(old)
    ensure_empty_msgstr!(new)
    %Translation{
      msgid: old.msgid,
      msgstr: old.msgstr,
      # The new in-memory translation has no comments since it was extracted
      # from the source code.
      comments: old.comments,
      # We don't care about the references of the old translation since the new
      # in-memory translation has all the actual and current references.
      references: new.references,
    }
  end

  defp merge_translations(%PluralTranslation{} = old, %PluralTranslation{comments: []} = new) do
    ensure_empty_msgstr!(old)
    ensure_empty_msgstr!(new)
    # The logic here is the same as for %Translation{}s.
    %PluralTranslation{
      msgid: old.msgid,
      msgid_plural: old.msgid_plural,
      msgstr: old.msgstr,
      comments: old.comments,
      references: new.references,
    }
  end

  defp ensure_empty_msgstr!(%Translation{msgstr: msgstr} = t) do
    unless blank?(msgstr) do
      raise Error, "translation with msgid '#{IO.iodata_to_binary(t.msgid)}' has a non-empty msgstr"
    end
  end

  defp ensure_empty_msgstr!(%PluralTranslation{msgstr: %{0 => str0, 1 => str1}} = t) do
    if not blank?(str0) or not blank?(str1) do
      raise Error,
        "plural translation with msgid '#{IO.iodata_to_binary(t.msgid)}' has a non-empty msgstr"
    end
  end

  defp ensure_empty_msgstr!(%PluralTranslation{} = t) do
    raise Error,
      "plural translation with msgid '#{IO.iodata_to_binary(t.msgid)}' has a non-empty msgstr"
  end

  defp blank?(nil), do: true
  defp blank?(str), do: IO.iodata_length(str) == 0
end
