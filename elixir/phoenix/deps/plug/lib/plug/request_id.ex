defmodule Plug.RequestId do
  @moduledoc """
  A plug for generating a unique request id for each request. A generated
  request id will in the format "uq8hs30oafhj5vve8ji5pmp7mtopc08f".

  If a request id already exists as the "x-request-id" HTTP request header,
  then that value will be used assuming it is between 20 and 200 characters.
  If it is not, a new request id will be generated.

  The request id is added to the Logger metadata as `:request_id` and the response as
  the "x-request-id" HTTP header. To see the request id in your log output,
  configure your logger backends to include the `:request_id` metadata:

      config :logger, :console, metadata: [:request_id]

  It is recommended to include this metadata configuration in your production
  configuration file.

  To use it, just plug it into the desired module:

      plug Plug.RequestId

  ## Options

    * `:http_header` - The name of the HTTP *request* header to check for
      existing request ids. This is also the HTTP *response* header that will be
      set with the request id. Default value is "x-request-id"

        plug Plug.RequestId, http_header: "custom-request-id"
  """

  require Logger
  alias Plug.Conn
  @behaviour Plug

  def init(opts) do
    Keyword.get(opts, :http_header, "x-request-id")
  end

  def call(conn, req_id_header) do
    conn
    |> get_request_id(req_id_header)
    |> set_request_id(req_id_header)
  end

  defp get_request_id(conn, header) do
    case Conn.get_req_header(conn, header) do
      []      -> {conn, generate_request_id()}
      [val|_] -> if valid_request_id?(val), do: {conn, val}, else: {conn, generate_request_id()}
    end
  end

  defp set_request_id({conn, request_id}, header) do
    Logger.metadata(request_id: request_id)
    Conn.put_resp_header(conn, header, request_id)
  end

  defp generate_request_id do
    Base.hex_encode32(:crypto.strong_rand_bytes(20), case: :lower)
  end

  defp valid_request_id?(s), do: byte_size(s) in 20..200
end
