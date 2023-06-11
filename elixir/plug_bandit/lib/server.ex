defmodule Server do
  use Plug.Router

  @compile :native
  @compile {:hipe, [:verbose, :o3]}

  plug :match
  plug :dispatch

  get "/" do
    send_resp(conn, 200, "")
  end

  get "/user/:id" do
    send_resp(conn, 200, id)
  end

  post "/user" do
    send_resp(conn, 200, "")
  end
end
