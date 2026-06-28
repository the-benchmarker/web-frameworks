defmodule Server do
  @moduledoc """
  HTTP Request Handler for Plug Bandit Server
  
  Handles incoming HTTP requests using Plug Router on Bandit server.
  Implements production-grade security headers and proper error handling.
  """
  
  use Plug.Router

  @compile :native
  @compile {:hipe, [:o3]}

  # ===========================================================================
  # Security Headers Plug
  # ===========================================================================
  # Adds security headers to every response
  
  defp security_headers(conn, _opts) do
    conn
    |> put_resp_header("Server", "Plug Bandit")
    |> put_resp_header("X-Content-Type-Options", "nosniff")
    |> put_resp_header("X-Frame-Options", "DENY")
    |> put_resp_header("X-XSS-Protection", "1; mode=block")
    |> put_resp_header("Strict-Transport-Security", "max-age=63072000; includeSubDomains; preload")
    |> put_resp_header("Content-Security-Policy", "default-src 'self'")
    |> put_resp_header("Referrer-Policy", "strict-origin-when-cross-origin")
    |> put_resp_header("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
    |> put_resp_header("Cache-Control", "no-store, no-cache, must-revalidate, private")
  end

  # ===========================================================================
  # Request ID Plug - For tracing and logging
  # ===========================================================================
  
  defp request_id(conn, _opts) do
    request_id = conn.params["request_id"] || UUID.uuid4()
    assign(conn, :request_id, request_id)
  end

  # ===========================================================================
  # Error Handling
  # ===========================================================================
  
  defp handle_errors(conn, _opts) do
    case conn.status do
      404 -> send_resp(conn, 404, "Not Found")
      405 -> send_resp(conn, 405, "Method Not Allowed")
      _ -> conn
    end
  end

  # ===========================================================================
  # Plug Pipeline
  # ===========================================================================

  plug :match
  plug :dispatch
  plug :request_id
  plug :security_headers
  plug :handle_errors

  # ===========================================================================
  # Route Handlers
  # ===========================================================================

  get "/", do: send_resp(conn, 200, "")
  get "/user/:id", do: send_resp(conn, 200, id)
  post "/user", do: send_resp(conn, 201, "")
end
