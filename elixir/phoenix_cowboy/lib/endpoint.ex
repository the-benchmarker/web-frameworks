defmodule Server.Endpoint do
  @moduledoc """
  Phoenix Endpoint Module for Plug Cowboy Server
  
  Handles HTTP requests and responses with security best practices.
  Implements production-grade security headers and error handling.
  """
  
  use Phoenix.Endpoint, otp_app: :server

  @compile :native
  @compile {:hipe, [:o3]}

  # ===========================================================================
  # Security Headers Configuration
  # ===========================================================================
  # These headers are added to every response for security
  
  plug :protect_from_forgery
  plug :put_secure_browser_headers

  # ===========================================================================
  # Custom Security Headers
  # ===========================================================================
  
  defp put_custom_security_headers(conn, _opts) do
    conn
    |> put_resp_header("Server", "Phoenix Plug Cowboy")
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
  # Request ID for tracing and logging
  # ===========================================================================
  
  defp request_id(conn, _opts) do
    request_id = get_req_header(conn, "x-request-id") || UUID.uuid4()
    conn
    |> assign(:request_id, request_id)
    |> put_resp_header("X-Request-ID", request_id)
  end

  # ===========================================================================
  # Plug Pipeline
  # ===========================================================================

  plug :put_custom_security_headers
  plug :request_id
  plug Server.Router
end
