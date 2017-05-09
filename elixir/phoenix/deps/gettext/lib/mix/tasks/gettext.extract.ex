defmodule Mix.Tasks.Gettext.Extract do
  use Mix.Task
  @recursive true

  @shortdoc "Extracts translations from source code"

  @moduledoc """
  Extracts translations by recompiling the Elixir source code.

      mix gettext.extract [OPTIONS]

  Translations are extracted into POT (Portable Object Template) files (with a
  `.pot` extension). The location of these files is determined by the `:otp_app`
  and `:priv` options given by gettext modules when they call `use Gettext`. One
  POT file is generated for each translation domain.

  It is possible to give the `--merge` option to perform merging
  for every Gettext backend updated during merge:

      mix gettext.extract --merge

  All other options passed to `gettext.extract` are forwarded to the
  `gettext.merge` task (`Mix.Tasks.Gettext.Merge`), which is called internally
  by this task. For example:

      mix gettext.extract --merge --no-fuzzy

  """
  def run(args) do
    Application.ensure_all_started(:gettext)
    _ = Mix.Project.get!
    config = Mix.Project.config()
    pot_files = extract(config[:app], config[:gettext] || [])

    case args do
      [] ->
        write_extracted_files(pot_files)
      ["--merge"] ->
        write_extracted_files(pot_files)
        run_merge(pot_files, args)
      _ ->
        Mix.raise "The gettext.extract task only supports the --merge option. " <>
                  "See `mix help gettext.extract` for more information"
    end

    :ok
  end

  defp write_extracted_files(pot_files) do
    for {path, contents} <- pot_files do
      Task.async fn ->
        File.mkdir_p!(Path.dirname(path))
        File.write!(path, contents)
        Mix.shell.info "Extracted #{Path.relative_to_cwd(path)}"
      end
    end |> Enum.map(&Task.await/1)
  end

  defp extract(app, gettext_config) do
    Gettext.Extractor.enable
    force_compile()
    Gettext.Extractor.pot_files(app, gettext_config)
  after
    Gettext.Extractor.disable
  end

  defp force_compile do
    Enum.map Mix.Tasks.Compile.Elixir.manifests, &make_old_if_exists/1
    # If "compile" was never called, the reenabling is a no-op and
    # "compile.elixir" is a no-op as well (because it wasn't reenabled after
    # running "compile"). If "compile" was already called, then running
    # "compile" is a no-op and running "compile.elixir" will work because we
    # manually reenabled it.
    Mix.Task.reenable "compile.elixir"
    Mix.Task.run "compile"
    Mix.Task.run "compile.elixir"
  end

  defp make_old_if_exists(path) do
    :file.change_time(path, {{2000, 1, 1}, {0, 0, 0}})
  end

  defp run_merge(pot_files, argv) do
    pot_files
    |> Enum.map(fn {path, _} -> Path.dirname(path) end)
    |> Enum.uniq
    |> Enum.map(&Task.async(fn -> Mix.Tasks.Gettext.Merge.run([&1 | argv]) end))
    |> Enum.map(&Task.await/1)
  end
end
