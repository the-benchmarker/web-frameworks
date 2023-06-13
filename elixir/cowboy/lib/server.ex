defmodule Server do
  @compile :native
  @compile {:hipe, [:verbose, :o3]}

  def init(%{method: "GET", path: "/"} = request, state) do
    {:ok, :cowboy_req.reply(200, %{}, "", request), state}
  end

  def init(%{method: "GET", bindings: %{id: id}} = request, state) do
    {:ok, :cowboy_req.reply(200, %{}, id, request), state}
  end

  def init(%{method: "POST", path: "/user"} = request, state) do
    {:ok, :cowboy_req.reply(200, %{}, "", request), state}
  end

  def routes do
    [
      {"/", __MODULE__, []},
      {"/user", __MODULE__, []},
      {"/user/:id", __MODULE__, []}
    ]
  end
end
