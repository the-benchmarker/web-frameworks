defmodule Server.Controller do
  use Phoenix.Controller, namespace: Server

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
