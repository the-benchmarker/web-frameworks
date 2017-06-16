defmodule MyElli.Callback do
  @behaviour :elli_handler

  def handle(req, _args) do
    do_handle(:elli_request.method(req), :elli_request.path(req))
  end

  defp do_handle(:GET, []), do: {:ok, ""}
  defp do_handle(:GET, ["user", id]), do: {:ok, id}
  defp do_handle(:POST, ["user"]), do: {:ok, ""}

  def handle_event(_event, _data, _args), do: :ok
end
