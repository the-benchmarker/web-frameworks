defmodule Plug.UploadError do
  defexception [:message]
end

defmodule Plug.Upload do
  @moduledoc """
  A server (a `GenServer` specifically) that manages uploaded files.

  Uploaded files are stored in a temporary directory
  and removed from that directory after the process that
  requested the file dies.

  During the request, files are represented with
  a `Plug.Upload` struct that contains three fields:

    * `:path` - the path to the uploaded file on the filesystem
    * `:content_type` - the content type of the uploaded file
    * `:filename` - the filename of the uploaded file given in the request

  **Note**: as mentioned in the documentation for `Plug.Parsers`, the `:plug`
  application has to be started in order to upload files and use the
  `Plug.Upload` module.
  """

  use GenServer
  defstruct [:path, :content_type, :filename]

  @type t :: %__MODULE__{
    path: Path.t,
    filename: binary,
    content_type: binary | nil
  }

  @table __MODULE__
  @max_attempts 10
  @temp_env_vars ~w(PLUG_TMPDIR TMPDIR TMP TEMP)s

  @doc """
  Requests a random file to be created in the upload directory
  with the given prefix.
  """
  @spec random_file(binary) ::
        {:ok, binary} |
        {:too_many_attempts, binary, pos_integer} |
        {:no_tmp, [binary]}
  def random_file(prefix) do
    case ensure_tmp() do
      {:ok, tmp, paths} ->
        open_random_file(prefix, tmp, 0, paths)
      {:no_tmp, tmps} ->
        {:no_tmp, tmps}
    end
  end

  defp ensure_tmp() do
    pid = self()
    server = plug_server()

    case :ets.lookup(@table, pid) do
      [{^pid, tmp, paths}] ->
        {:ok, tmp, paths}
      [] ->
        {:ok, tmps} = GenServer.call(server, :upload)
        {mega, _, _} = :os.timestamp
        subdir = "/plug-" <> i(mega)

        if tmp = Enum.find_value(tmps, &make_tmp_dir(&1 <> subdir)) do
          true = :ets.insert_new(@table, {pid, tmp, []})
          {:ok, tmp, []}
        else
          {:no_tmp, tmps}
        end
    end
  end

  defp make_tmp_dir(path) do
    case File.mkdir_p(path) do
      :ok -> path
      {:error, _} -> nil
    end
  end

  defp open_random_file(prefix, tmp, attempts, paths) when attempts < @max_attempts do
    path = path(prefix, tmp)

    case :file.write_file(path, "", [:write, :raw, :exclusive, :binary]) do
      :ok ->
        :ets.update_element(@table, self(), {3, [path|paths]})
        {:ok, path}
      {:error, reason} when reason in [:eexist, :eacces] ->
        open_random_file(prefix, tmp, attempts + 1, paths)
    end
  end

  defp open_random_file(_prefix, tmp, attempts, _paths) do
    {:too_many_attempts, tmp, attempts}
  end

  defp path(prefix, tmp) do
    {_mega, sec, micro} = :os.timestamp
    scheduler_id = :erlang.system_info(:scheduler_id)
    tmp <> "/" <> prefix <> "-" <> i(sec) <> "-" <> i(micro) <> "-" <> i(scheduler_id)
  end

  @compile {:inline, i: 1}
  defp i(integer), do: Integer.to_string(integer)

  @doc """
  Requests a random file to be created in the upload directory
  with the given prefix. Raises on failure.
  """
  @spec random_file!(binary) :: binary | no_return
  def random_file!(prefix) do
    case random_file(prefix) do
      {:ok, path} ->
        path
      {:too_many_attempts, tmp, attempts} ->
        raise Plug.UploadError, "tried #{attempts} times to create an uploaded file at #{tmp} but failed. " <>
                                "Set PLUG_TMPDIR to a directory with write permission"
      {:no_tmp, _tmps} ->
        raise Plug.UploadError, "could not create a tmp directory to store uploads. " <>
                                "Set PLUG_TMPDIR to a directory with write permission"
    end
  end

  defp plug_server do
    Process.whereis(__MODULE__) ||
      raise Plug.UploadError, "could not find process Plug.Upload. Have you started the :plug application?"
  end

  @doc """
  Starts the upload handling server.
  """
  def start_link() do
    GenServer.start_link(__MODULE__, :ok, [name: __MODULE__])
  end

  ## Callbacks

  def init(:ok) do
    tmp = Enum.find_value @temp_env_vars, "/tmp", &System.get_env/1
    cwd = Path.join(File.cwd!, "tmp")
    :ets.new(@table, [:named_table, :public, :set])
    {:ok, [tmp, cwd]}
  end

  def handle_call(:upload, {pid, _ref}, dirs) do
    Process.monitor(pid)
    {:reply, {:ok, dirs}, dirs}
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    case :ets.lookup(@table, pid) do
      [{pid, _tmp, paths}] ->
        :ets.delete(@table, pid)
        Enum.each paths, &:file.delete/1
      [] ->
        :ok
    end
    {:noreply, state}
  end

  def handle_info(msg, state) do
    super(msg, state)
  end
end
