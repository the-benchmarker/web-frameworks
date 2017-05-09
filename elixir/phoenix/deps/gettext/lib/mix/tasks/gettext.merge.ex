defmodule Mix.Tasks.Gettext.Merge do
  use Mix.Task
  @recursive true

  @shortdoc "Merge template files into translation files"

  @moduledoc """
  Merges PO/POT files with PO files.

  This task is used when translations in the source code change: when they do,
  `mix gettext.extract` is usually used to extract the new translations to POT
  files. At this point, developers or translators can use this task to "sync"
  the newly updated POT files with the existing locale-specific PO files. All
  the metadata for each translation (like position in the source code, comments
  and so on) is taken from the newly updated POT file; the only things taken
  from the PO file are the actual translated strings.

  #### Fuzzy matching

  Translations in the updated PO/POT file that have an exact match (a
  translation with the same msgid) in the old PO file are merged as described
  above. When a translation in the update PO/POT files has no match in the old
  PO file, a fuzzy match for that translation is attempted. For example, assume
  we have this POT file:

      msgid "hello, world!"
      msgstr ""

  and we merge it with this PO file:

      # notice no exclamation point here
      msgid "hello, world"
      msgstr "ciao, mondo"

  Since the two translations are very similar, the msgstr from the existing
  translation will be taken over to the new translation, which will however be
  marked as *fuzzy*:

      #, fuzzy
      msgid "hello, world!"
      msgstr "ciao, mondo!"

  Generally, a `fuzzy` flag calls for review from a translator.

  Fuzzy matching can be configured (e.g., the threshold for translation
  similarity can be tweaked) or disabled entirely; lool at the "Options" section
  below.

  ## Usage

      mix gettext.merge OLD_FILE UPDATED_FILE [OPTIONS]
      mix gettext.merge DIR [OPTIONS]

  If two files are given as arguments, they must be a `.po` file and a
  `.po`/`.pot` file. The first one is the old PO file, while the second one is
  the last generated one. They are merged and written over the first file. For
  example:

      mix gettext.merge priv/gettext/en/LC_MESSAGES/default.po priv/gettext/default.pot

  If only one argument is given, then that argument must be a directory
  containing gettext translations (with `.pot` files at the root level alongside
  locale directories - this is usually a "backend" directory used by a Gettext
  backend).

      mix gettext.merge priv/gettext

  If the `--locale LOCALE` option is given, then only the PO files in
  `DIR/LOCALE/LC_MESSAGES` will be merged with the POT files in `DIR`. If no
  options are given, then all the PO files for all locales under `DIR` are
  merged with the POT files in `DIR`.

  ## Options

  The `--locale` option can only be given when there's only one argument (a
  directory). These options can always be passed to `gettext.merge`:

    * `--no-fuzzy` - stops fuzzy matching from being performed when merging
      files.
    * `--fuzzy-threshold` - a float between `0` and `1` which represents the
      miminum Jaro distance needed for two translations to be considered a fuzzy
      match. Overrides the global `:fuzzy_threshold` option (see the docs for
      `Gettext` for more information on this option).

  """

  @default_fuzzy_threshold 0.8

  alias Gettext.Merger

  def run(args) do
    _ = Mix.Project.get!
    gettext_config = Mix.Project.config()[:gettext] || []

    parse_switches = [locale: :string, fuzzy: :boolean, fuzzy_threshold: :float]
    case OptionParser.parse(args, switches: parse_switches) do
      {opts, [arg1, arg2], _} ->
        run_with_two_args(arg1, arg2, opts, gettext_config)
      {opts, [arg], _} ->
        run_with_one_arg(arg, opts, gettext_config)
      {_, [], _} ->
        Mix.raise "gettext.merge requires at least one argument to work. " <>
                  "Use `mix help gettext.merge` to see the usage of this task"
      {_, _, [_ | _] = errors} ->
        for {key, _} <- errors, do: Mix.shell.error "#{key} is invalid"
        Mix.raise "`mix gettext.merge` aborted"
      {_, _, _} ->
        Mix.raise "Too many arguments for the gettext.merge task. " <>
                  "Use `mix help gettext.merge` to see the usage of this task"
    end

    Mix.Task.reenable("gettext.merge")
  end

  defp run_with_two_args(arg1, arg2, opts, gettext_config) do
    merging_opts = validate_merging_opts!(opts, gettext_config)

    if Path.extname(arg1) == ".po" and Path.extname(arg2) in [".po", ".pot"] do
      ensure_file_exists!(arg1)
      ensure_file_exists!(arg2)
      {path, contents} = merge_po_with_pot(arg1, arg2, merging_opts)
      File.write!(path, contents)
      Mix.shell.info "Wrote #{path}"
    else
      Mix.raise "Arguments must be a PO file and a PO/POT file"
    end
  end

  defp run_with_one_arg(arg, opts, gettext_config) do
    ensure_dir_exists!(arg)
    merging_opts = validate_merging_opts!(opts, gettext_config)

    if locale = opts[:locale] do
      merge_locale_dir(arg, locale, merging_opts)
    else
      merge_all_locale_dirs(arg, merging_opts)
    end
  end

  defp merge_po_with_pot(po_file, pot_file, opts) do
    {po_file, Merger.merge_files(po_file, pot_file, opts)}
  end

  defp merge_locale_dir(pot_dir, locale, opts) do
    locale_dir = locale_dir(pot_dir, locale)
    create_missing_locale_dir(locale_dir)
    merge_dirs(locale_dir, pot_dir, opts)
  end

  defp merge_all_locale_dirs(pot_dir, opts) do
    pot_dir
    |> ls_locale_dirs
    |> Enum.each(&merge_dirs(&1, pot_dir, opts))
  end

  def locale_dir(pot_dir, locale) do
    Path.join([pot_dir, locale, "LC_MESSAGES"])
  end

  defp ls_locale_dirs(dir) do
    dir
    |> File.ls!
    |> Enum.filter(&File.dir?(Path.join(dir, &1)))
    |> Enum.map(&locale_dir(dir, &1))
  end

  defp merge_dirs(po_dir, pot_dir, opts) do
    pot_dir
    |> Path.join("*.pot")
    |> Path.wildcard()
    |> Enum.map(fn pot_file ->
      Task.async fn ->
        pot_file
        |> find_matching_po(po_dir)
        |> merge_or_create(opts)
        |> write_file()
      end
    end)
    |> Enum.map(&Task.await/1)

    # Now warn for every PO file that has no matching POT file.
    po_dir
    |> Path.join("*.po")
    |> Path.wildcard()
    |> Enum.reject(&po_has_matching_pot?(&1, pot_dir))
    |> Enum.each(&warn_for_missing_pot_file(&1, pot_dir))
  end

  defp find_matching_po(pot_file, po_dir) do
    domain = Path.basename(pot_file, ".pot")
    {pot_file, Path.join(po_dir, "#{domain}.po")}
  end

  defp merge_or_create({pot_file, po_file}, opts) do
    if File.regular?(po_file) do
      {po_file, Merger.merge_files(po_file, pot_file, opts)}
    else
      {po_file, Merger.new_po_file(po_file, pot_file)}
    end
  end

  defp write_file({path, contents}) do
    File.write!(path, contents)
    Mix.shell.info "Wrote #{path}"
  end

  defp po_has_matching_pot?(po_file, pot_dir) do
    domain = Path.basename(po_file, ".po")
    pot_path = Path.join(pot_dir, "#{domain}.pot")
    File.exists?(pot_path)
  end

  defp warn_for_missing_pot_file(po_file, pot_dir) do
    Mix.shell.info "Warning: PO file #{po_file} has no matching POT file in #{pot_dir}"
  end

  defp ensure_file_exists!(path) do
    unless File.regular?(path), do: Mix.raise("No such file: #{path}")
  end

  defp ensure_dir_exists!(path) do
    unless File.dir?(path), do: Mix.raise("No such directory: #{path}")
  end

  defp create_missing_locale_dir(dir) do
    unless File.dir?(dir) do
      File.mkdir_p!(dir)
      Mix.shell.info "Created directory #{dir}"
    end
  end

  defp validate_merging_opts!(opts, gettext_config) do
    default_threshold = gettext_config[:fuzzy_threshold] || @default_fuzzy_threshold
    defaults = [fuzzy: true, fuzzy_threshold: default_threshold]
    opts = Keyword.merge(defaults, Keyword.take(opts, [:fuzzy, :fuzzy_threshold]))

    threshold = opts[:fuzzy_threshold]
    unless threshold >= 0.0 and threshold <= 1.0 do
      Mix.raise "The :fuzzy_threshold option must be a float >= 0.0 and <= 1.0"
    end

    opts
  end
end
