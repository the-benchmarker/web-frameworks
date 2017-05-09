defmodule MyPhoenix.PageController do
  use MyPhoenix.Web, :controller

  def index(conn, _params) do
    send_resp conn, 200, ""
  end

  def show_user(conn, %{"id" => id}) do
    send_resp conn, 200, id
  end

  def create_user(conn, _params) do
    send_resp conn, 200, ""
  end
end
