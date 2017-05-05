defmodule MyPlug.Router do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/", do: send_resp(conn, 200, "")
  get "/user/:id", do: send_resp(conn, 200, id)
  post "/user", do: send_resp(conn, 200, "")
end
