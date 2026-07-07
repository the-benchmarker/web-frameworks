defmodule Server do
  @moduledoc """
  HTTP Request Handler for Cowboy Server
  
  Handles incoming HTTP requests with production-grade security headers
  and proper error handling.
  """
  
  @compile :native
  @compile {:hipe, [:o3]}
  
  # ===========================================================================
  # Security Headers
  # ===========================================================================
  # These headers are added to every response for security
  
  @security_headers %{
    "Server" => "Cowboy",
    "X-Content-Type-Options" => "nosniff",
    "X-Frame-Options" => "DENY",
    "X-XSS-Protection" => "1; mode=block",
    "Strict-Transport-Security" => "max-age=63072000; includeSubDomains; preload",
    "Content-Security-Policy" => "default-src 'self'",
    "Referrer-Policy" => "strict-origin-when-cross-origin",
    "Permissions-Policy" => "geolocation=(), microphone=(), camera=()",
    "Cache-Control" => "no-store, no-cache, must-revalidate, private"
  }

  @default_headers @security_headers |> Map.put("Content-Type", "text/plain; charset=utf-8")

  # ===========================================================================
  # Route Handlers
  # ===========================================================================

  @impl true
  def init(%{method: "GET", path: "/"} = request, state) do
    headers = @default_headers |> Map.put("Content-Length", "0")
    {:ok, :cowboy_req.reply(200, headers, "", request), state}
  end

  @impl true
  def init(%{method: "GET", bindings: %{id: id}} = request, state) do
    body = to_string(id)
    headers = @default_headers |> Map.put("Content-Length", Integer.to_string(byte_size(body)))
    {:ok, :cowboy_req.reply(200, headers, body, request), state}
  end

  @impl true
  def init(%{method: "POST", path: "/user"} = request, state) do
    headers = @default_headers |> Map.put("Content-Length", "0")
    {:ok, :cowboy_req.reply(201, headers, "", request), state}
  end

  # ===========================================================================
  # Error Handling - Handle unsupported methods
  # ===========================================================================

  @impl true
  def init(_request = %{path: "/"}, state) do
    headers = @default_headers |> Map.put("Content-Length", "9")
    body = "Method Not Allowed"
    {:ok, :cowboy_req.reply(405, headers, body, _request), state}
  end

  @impl true
  def init(_request = %{path: "/user"}, state) do
    headers = @default_headers |> Map.put("Content-Length", "9")
    body = "Method Not Allowed"
    {:ok, :cowboy_req.reply(405, headers, body, _request), state}
  end

  @impl true
  def init(_request, state) do
    headers = @default_headers |> Map.put("Content-Length", "13")
    body = "Not Found"
    {:ok, :cowboy_req.reply(404, headers, body, _request), state}
  end

  # ===========================================================================
  # Routing
  # ===========================================================================

  def routes do
    [
      # Health check endpoint
      {"/", __MODULE__, []},
      # User creation endpoint
      {"/user", __MODULE__, []},
      # User retrieval endpoint with ID parameter
      {"/user/:id", __MODULE__, []}
    ]
  end
end
