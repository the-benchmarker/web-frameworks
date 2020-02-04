defmodule Server do
  def start_link(port: port) do
    tcp_config = [
      :binary,
      backlog: 1000,
      active: false,
      packet: :http_bin,
      reuseaddr: true
    ]

    with {:ok, socket} <- :gen_tcp.listen(port, tcp_config) do
      {:ok, spawn_link(Server, :accept_loop, [socket])}
    end
  end

  def accept_loop(socket) do
    with {:ok, client} <- :gen_tcp.accept(socket),
         {:ok, pid} <- Task.start(fn -> serve(client) end) do
      accept_loop(socket)
    end
  end

  def serve(socket) do
    case :gen_tcp.recv(socket, 0) do
      {:ok, {:http_request, :GET, {:abs_path, "/"}, _}} ->
        send_resp(socket)

      {:ok, {:http_request, :POST, {:abs_path, "/user"}, _}} ->
        send_resp(socket)

      {:ok, {:http_request, :GET, {:abs_path, "/user/" <> id}, _}} ->
        send_resp(socket, id)

      {:error, :closed} ->
        :ok
    end
  end

  def send_resp(socket, body \\ "") do
    response = """
    HTTP/1.1 200 OK\r
    Content-Length: #{byte_size(body)}\r
    \r
    #{body}
    """

    :gen_tcp.send(socket, response)
    :gen_tcp.close(socket)
  end

  def child_spec(opts) do
    %{id: Server, start: {Server, :start_link, [opts]}}
  end
end
