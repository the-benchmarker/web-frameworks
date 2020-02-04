defmodule Server do
  use Plug.Router

  plug(:match)
  plug(:dispatch)

  get "/" do
    send_resp(conn, 200, "")
  end

  get "/user/:id" do
    with %{path_params: %{"id" => id}} <- conn do
      send_resp(conn, 200, id)
    end
  end

  post "/user" do
    send_resp(conn, 200, "")
  end
end
