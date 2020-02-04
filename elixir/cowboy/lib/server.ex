defmodule Server do
  @compile {:inline, response: 1, response: 2}
  @compile :native
  @compile {:hipe, [:o3]}

  def init(%{method: "GET", path: "/"} = request, state) do
    {:ok, response(request), state}
  end

  def init(%{method: "GET", bindings: %{id: id}} = request, state) do
    {:ok, response(request, id), state}
  end

  def init(%{method: "POST", path: "/user"} = request, state) do
    {:ok, response(request), state}
  end

  defp response(request, body \\ "") do
    :cowboy_req.reply(200, %{}, body, request)
  end

  def routes do
    [
      {"/", __MODULE__, []},
      {"/user", __MODULE__, []},
      {"/user/:id", __MODULE__, []}
    ]
  end
end
