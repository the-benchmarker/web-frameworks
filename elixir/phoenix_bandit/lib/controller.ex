defmodule Server.Controller do
  use Phoenix.Controller, namespace: Server

  @compile :native
  @compile {:hipe, [:verbose, :o3]}

  def index(conn, _params) do
    Plug.Conn.send_resp(conn, 200, "")
  end

  def show(conn, %{"id" => id}) do
    Plug.Conn.send_resp(conn, 200, id)
  end

  def create(conn, _params) do
    Plug.Conn.send_resp(conn, 200, "")
  end
end
