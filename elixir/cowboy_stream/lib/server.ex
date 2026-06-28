defmodule Server do
  @moduledoc """
  HTTP Stream Handler for Cowboy Stream Server
  
  Handles incoming HTTP requests using Cowboy's stream interface.
  Implements production-grade security headers and proper error handling.
  """
  
  @behaviour :cowboy_stream

  @compile :native
  @compile {:hipe, [:o3]}

  # ===========================================================================
  # Security Headers
  # ===========================================================================
  # These headers are added to every response for security
  
  @security_headers %{
    "Server" => "Cowboy Stream",
    "X-Content-Type-Options" => "nosniff",
    "X-Frame-Options" => "DENY",
    "X-XSS-Protection" => "1; mode=block",
    "Strict-Transport-Security" => "max-age=63072000; includeSubDomains; preload",
    "Content-Security-Policy" => "default-src 'self'",
    "Referrer-Policy" => "strict-origin-when-cross-origin",
    "Permissions-Policy" => "geolocation=(), microphone=(), camera=()",
    "Cache-Control" => "no-store, no-cache, must-revalidate, private"
  }

  @content_type "text/plain; charset=utf-8"

  # ===========================================================================
  # Request Handlers
  # ===========================================================================

  @impl true
  def init(_stream_id, %{method: "GET", path: "/"}, _opts) do
    {response(), []}
  end

  @impl true
  def init(_stream_id, %{method: "GET", path: "/user/" <> id}, _opts) do
    {response(to_string(id)), []}
  end

  @impl true
  def init(_stream_id, %{method: "POST", path: "/user"}, _opts) do
    {response(201), []}
  end

  # ===========================================================================
  # Response Helpers
  # ===========================================================================

  defp security_headers do
    @security_headers
  end

  defp response(body \ "") do
    content_length = Integer.to_string(byte_size(body))
    headers = security_headers() |> Map.put("Content-Type", @content_type) |> Map.put("Content-Length", content_length)
    
    [
      {:response, 200, headers, body},
      :stop
    ]
  end

  defp response(status_code, body \ "") do
    content_length = Integer.to_string(byte_size(body))
    headers = security_headers() |> Map.put("Content-Type", @content_type) |> Map.put("Content-Length", content_length)
    
    [
      {:response, status_code, headers, body},
      :stop
    ]
  end

  defp error_response(status_code, body) do
    content_length = Integer.to_string(byte_size(body))
    headers = security_headers() |> Map.put("Content-Type", @content_type) |> Map.put("Content-Length", content_length)
    
    [
      {:response, status_code, headers, body},
      :stop
    ]
  end

  # ===========================================================================
  # Error Handling - Handle unsupported methods
  # ===========================================================================

  @impl true
  def init(_stream_id, %{path: "/"}, _opts) do
    {error_response(405, "Method Not Allowed"), []}
  end

  @impl true
  def init(_stream_id, %{path: "/user"}, _opts) do
    {error_response(405, "Method Not Allowed"), []}
  end

  @impl true
  def init(_stream_id, _request, _opts) do
    {error_response(404, "Not Found"), []}
  end

  # ===========================================================================
  # Cowboy Stream Callbacks
  # ===========================================================================

  @impl true
  def data(_stream_id, _is_fin, _data, state), do: {[], state}

  @impl true
  def info(_stream_id, _info, state), do: {[], state}

  @impl true
  def terminate(_stream_id, _reason, _state), do: :ok

  @impl true
  def early_error(_stream_id, _reason, _partial_req, resp, _opts), do: resp
end
