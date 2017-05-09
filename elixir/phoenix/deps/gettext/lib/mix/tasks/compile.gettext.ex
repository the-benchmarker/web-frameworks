defmodule Mix.Tasks.Compile.Gettext do
  use Mix.Task

  @recursive true

  @moduledoc """
  Force Gettext modules to recompile once .po files change.
  """

  # This compiler finds .po files in the "priv" directory of the current
  # project, then builds a bunch of manifest files based on these .po files (one
  # manifest for each "prefix", which corresponds to a Gettext backend) and
  # touches these manifests in case they're stale.
  # What makes the Gettext backend recompile when .po files change though is
  # that each backend specifies the corresponding manifest file as an
  # @external_resource.

  @default_wildcard "gettext/*/LC_MESSAGES/*.po"

  def run(_, priv_dir \\ "priv") do
    Application.ensure_all_started(:gettext)
    _ = Mix.Project.get!
    app_dir = Mix.Project.app_path()
    gettext_config = Mix.Project.config()[:gettext] || []

    wildcard = gettext_config[:compiler_po_wildcard] || @default_wildcard

    changed =
      priv_dir
      |> Path.join(wildcard)
      |> Path.wildcard()
      |> Enum.group_by(&priv_prefix(&1, app_dir))
      |> Map.delete(:not_in_canonical_dir)
      |> change_manifests()

    if changed == [], do: :noop, else: :ok
  end

  defp priv_prefix(path, app_dir) do
    parts = Path.split(path)

    if index = Enum.find_index(parts, & &1 == "LC_MESSAGES") do
      filename =
        [".compile" | Enum.take(parts, index - 1)]
        |> Path.join()
        |> String.replace("/", "_")
      Path.join(app_dir, filename)
    else
      :not_in_canonical_dir
    end
  end

  defp change_manifests(manifest_to_pos) do
    for {manifest, pos} <- manifest_to_pos,
        manifest_stale?(manifest, pos),
        do: write_manifest(manifest, pos)
  end

  defp read_manifest(manifest) do
    case File.read(manifest) do
      {:ok, pos} -> String.split(pos, "\n", trim: true)
      {:error, _} -> []
    end
  end

  defp write_manifest(manifest, pos) do
    File.mkdir_p! Path.dirname(manifest)
    File.write! manifest, pos |> Enum.sort |> Enum.map(&[&1, ?\n])
    manifest
  end

  defp manifest_stale?(manifest, pos) do
    current = read_manifest(manifest)
    added   = pos -- current
    removed = current -- pos

    if added == [] and removed == [] do
      Mix.Utils.stale?(pos, [manifest])
    else
      true
    end
  end
end
