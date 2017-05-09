defmodule Plug.Conn.Status do
  @moduledoc """
  Conveniences for working with status codes.
  """

  custom_statuses = Application.get_env(:plug, :statuses, %{})

  statuses = %{
    100 => "Continue",
    101 => "Switching Protocols",
    102 => "Processing",
    200 => "OK",
    201 => "Created",
    202 => "Accepted",
    203 => "Non-Authoritative Information",
    204 => "No Content",
    205 => "Reset Content",
    206 => "Partial Content",
    207 => "Multi-Status",
    208 => "Already Reported",
    226 => "IM Used",
    300 => "Multiple Choices",
    301 => "Moved Permanently",
    302 => "Found",
    303 => "See Other",
    304 => "Not Modified",
    305 => "Use Proxy",
    306 => "Switch Proxy",
    307 => "Temporary Redirect",
    308 => "Permanent Redirect",
    400 => "Bad Request",
    401 => "Unauthorized",
    402 => "Payment Required",
    403 => "Forbidden",
    404 => "Not Found",
    405 => "Method Not Allowed",
    406 => "Not Acceptable",
    407 => "Proxy Authentication Required",
    408 => "Request Timeout",
    409 => "Conflict",
    410 => "Gone",
    411 => "Length Required",
    412 => "Precondition Failed",
    413 => "Request Entity Too Large",
    414 => "Request-URI Too Long",
    415 => "Unsupported Media Type",
    416 => "Requested Range Not Satisfiable",
    417 => "Expectation Failed",
    418 => "I'm a teapot",
    421 => "Misdirected Request",
    422 => "Unprocessable Entity",
    423 => "Locked",
    424 => "Failed Dependency",
    425 => "Unordered Collection",
    426 => "Upgrade Required",
    428 => "Precondition Required",
    429 => "Too Many Requests",
    431 => "Request Header Fields Too Large",
    500 => "Internal Server Error",
    501 => "Not Implemented",
    502 => "Bad Gateway",
    503 => "Service Unavailable",
    504 => "Gateway Timeout",
    505 => "HTTP Version Not Supported",
    506 => "Variant Also Negotiates",
    507 => "Insufficient Storage",
    508 => "Loop Detected",
    510 => "Not Extended",
    511 => "Network Authentication Required"
  }

  reason_phrase_to_atom = fn reason_phrase ->
    reason_phrase
    |> String.downcase()
    |> String.replace("'", "")
    |> String.replace(~r/[^a-z0-9]/, "_")
    |> String.to_atom()
  end

  status_map_to_doc = fn statuses ->
    statuses
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map(fn {code, reason_phrase} ->
      atom = reason_phrase_to_atom.(reason_phrase)
      "  * `#{inspect atom}` - #{code}\n"
    end)
  end

  custom_status_doc =
    if custom_statuses != %{} do
      """
      ## Custom status codes

      #{status_map_to_doc.(custom_statuses)}
      """
    end

  @doc """
  Returns the status code given an integer or a known atom.

  ## Known status codes

  The following status codes can be given as atoms with their
  respective value shown next:

  #{status_map_to_doc.(statuses)}
  #{custom_status_doc}
  """
  @spec code(integer | atom) :: integer
  def code(integer_or_atom)

  def code(integer) when integer in 100..999 do
    integer
  end

  for {code, reason_phrase} <- statuses do
    atom = reason_phrase_to_atom.(reason_phrase)
    def code(unquote(atom)), do: unquote(code)
  end

  # This ensures that both the default and custom statuses will work
  for {code, reason_phrase} <- custom_statuses do
    atom = reason_phrase_to_atom.(reason_phrase)
    def code(unquote(atom)), do: unquote(code)
  end

  @spec reason_phrase(integer) :: String.t
  def reason_phrase(integer)

  for {code, phrase} <- Map.merge(statuses, custom_statuses) do
    def reason_phrase(unquote(code)), do: unquote(phrase)
  end

  def reason_phrase(code) do
    raise ArgumentError, """
    unknown status code #{inspect code}

    Custom codes can be defined in the configuration for the :plug application,
    under the :statuses key (which contains a map of status codes as keys and
    reason phrases as values). For example:

        config :plug, :statuses, %{451 => "Unavailable For Legal Reasons"}

    After defining the config for custom statuses, Plug must be recompiled for
    the changes to take place using:

        MIX_ENV=dev mix deps.compile plug

    Doing this will allow the use of the integer status code 451 as
    well as the atom :unavailable_for_legal_reasons in many Plug functions.
    For example:

        put_status(conn, :unavailable_for_legal_reasons)
    """
  end
end
