defmodule Server.Controller do
  use Phoenix.Controller, namespace: Server

  @compile :native
  @compile {:hipe, [:verbose, :o3]}

  def index(conn, _params) do
    text(conn, "")
  end

  def show(conn, %{"id" => id}) do
    text(conn, id)
  end

  def create(conn, _params) do
    text(conn, "")
  end
end
