defmodule Server do
  use GenServer

  def start_link(ref, transport, _opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [ref, transport])}
  end

  def init(args) do
    {:ok, args}
  end

  def init(ref, transport) do
    with {:ok, socket} <- :ranch.handshake(ref),
         :ok <- transport.setopts(socket, active: true) do
      :gen_server.enter_loop(__MODULE__, [], %{socket: socket, transport: transport})
    end
  end

  def handle_info({:tcp, socket, "GET / " <> _rest}, %{transport: transport} = state) do
    send_response(transport, socket)
    {:noreply, state}
  end

  def handle_info({:tcp, socket, "GET /user/" <> rest}, %{transport: transport} = state) do
    id = get_id(rest)

    send_response(transport, socket, id)
    {:noreply, state}
  end

  def handle_info({:tcp, socket, "POST /user " <> _rest}, %{transport: transport} = state) do
    send_response(transport, socket)
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, %{transport: transport} = state) do
    transport.close(socket)
    {:stop, :normal, state}
  end

  defp send_response(transport, socket, data \\ "") do
    response = """
    HTTP/1.1 200 OK\r
    Content-Length: #{byte_size(data)}\r
    \r
    #{data}
    """

    transport.send(socket, response)
  end

  defp get_id(string, id \\ "")

  defp get_id(<<" ", _rest::binary>>, id), do: id

  defp get_id(<<x::binary-size(1), rest::binary>>, id) do
    get_id(rest, id <> x)
  end
end
