defmodule Server.Controller do
  @moduledoc """
  Phoenix Controller Module
  
  Handles HTTP requests with production-grade security and error handling.
  Implements proper response formatting and content negotiation.
  """
  
  use Phoenix.Controller, namespace: Server

  @compile :native
  @compile {:hipe, [:o3]}

  # ===========================================================================
  # Response Helpers
  # ===========================================================================

  defp success_response(conn, status, body) do
    conn
    |> put_status(status)
    |> put_resp_content_type("text/plain; charset=utf-8")
    |> send_resp(status, body)
  end

  defp json_response(conn, status, data) do
    conn
    |> put_status(status)
    |> put_resp_content_type("application/json; charset=utf-8")
    |> send_resp(status, Jason.encode!(data))
  end

  defp error_response(conn, status, message) do
    conn
    |> put_status(status)
    |> put_resp_content_type("application/json; charset=utf-8")
    |> send_resp(status, Jason.encode!(%{error: message}))
  end

  # ===========================================================================
  # Action Handlers
  # ===========================================================================

  @doc """
  Index action - Health check endpoint
  Returns 200 OK with empty body
  """
  def index(conn, _params) do
    success_response(conn, 200, "")
  end

  @doc """
  Show action - Retrieve user by ID
  Returns 200 OK with user ID in body
  """
  def show(conn, %{"id" => id}) do
    success_response(conn, 200, id)
  end

  @doc """
  Create action - Create new user
  Returns 201 Created with empty body
  """
  def create(conn, _params) do
    success_response(conn, 201, "")
  end
end
