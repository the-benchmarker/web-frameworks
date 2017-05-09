defmodule Plug.CSRFProtection do
  @moduledoc """
  Plug to protect from cross-site request forgery.

  For this plug to work, it expects a session to have been
  previously fetched. It will then compare the plug stored
  in the session with the one sent by the request to determine
  the validity of the request. For an invalid request the action
  taken is based on the `:with` option.

  The token may be sent by the request either via the params
  with key "_csrf_token" or a header with name "x-csrf-token".

  GET requests are not protected, as they should not have any
  side-effect or change your application state. JavaScript
  requests are an exception: by using a script tag, external
  websites can embed server-side generated JavaScript, which
  can leak information. For this reason, this plug also forbids
  any GET JavaScript request that is not XHR (or AJAX).

  ## Token generation

  This plug won't generate tokens automatically. Instead,
  tokens will be generated only when required by calling
  `Plug.CSRFProtection.get_csrf_token/0`. The token is then
  stored in the process dictionary to be set in the request.

  One may wonder: why the process dictionary?

  The CSRF token is usually generated inside forms which may
  be isolated from the connection. Storing them in the process
  dictionary allows them to be generated as a side-effect,
  becoming one of those rare situations where using the process
  dictionary is useful.

  ## Options

    * `:with` - should be one of `:exception` or `:clear_session`. Defaults to
    `:exception`.
      * `:exception` -  for invalid requests, this plug will raise
      `Plug.CSRFProtection.InvalidCSRFTokenError`.
      * `:clear_session` -  for invalid requests, this plug will set an empty
      session for only this request. Also any changes to the session during this
      request will be ignored.

  ## Disabling

  You may disable this plug by doing
  `Plug.Conn.put_private(:plug_skip_csrf_protection, true)`. This was made
  available for disabling `Plug.CSRFProtection` in tests and not for dynamically
  skipping `Plug.CSRFProtection` in production code. If you want specific routes to
  skip `Plug.CSRFProtection`, then use a different stack of plugs for that route that
  does not include `Plug.CSRFProtection`.

  ## Examples

      plug Plug.Session, ...
      plug :fetch_session
      plug Plug.CSRFProtection

  """

  import Plug.Conn
  require Bitwise
  @unprotected_methods ~w(HEAD GET OPTIONS)

  defmodule InvalidCSRFTokenError do
    @moduledoc "Error raised when CSRF token is invalid."

    message =
      "invalid CSRF (Cross Site Request Forgery) token, make sure all "
      <> "requests include a valid '_csrf_token' param or 'x-csrf-token' header"

    defexception message: message, plug_status: 403
  end

  defmodule InvalidCrossOriginRequestError do
    @moduledoc "Error raised when non-XHR requests are used for Javascript responses."

    message =
      "security warning: an embedded <script> tag on another site requested "
      <> "protected JavaScript (if you know what you're doing, disable forgery "
      <> "protection for this route)"

    defexception message: message, plug_status: 403
  end

  ## API

  @doc """
  Gets the CSRF token.

  Generates a token and stores it in the process
  dictionary if one does not exist.
  """
  def get_csrf_token do
    if token = Process.get(:plug_masked_csrf_token) do
      token
    else
      token = mask(unmasked_csrf_token())
      Process.put(:plug_masked_csrf_token, token)
      token
    end
  end

  @doc """
  Deletes the CSRF token from the process dictionary.

  This will force the token to be deleted once the response is sent.
  """
  def delete_csrf_token do
    Process.delete(:plug_unmasked_csrf_token)
    Process.delete(:plug_masked_csrf_token)
  end

  ## Plug

  @behaviour Plug
  @token_size 16
  @encoded_token_size 24
  @double_encoded_token_size 32

  def init(opts), do: Keyword.get(opts, :with, :exception)

  def call(conn, mode) do
    csrf_token = get_csrf_from_session(conn)
    Process.put(:plug_unmasked_csrf_token, csrf_token)

    conn =
      cond do
        verified_request?(conn, csrf_token) ->
          conn
        mode == :clear_session ->
          conn |> configure_session(ignore: true) |> clear_session()
        mode == :exception ->
          raise InvalidCSRFTokenError
        true ->
          raise ArgumentError, "option :with should be one of :exception or :clear_session, got #{inspect mode}"
      end

    register_before_send(conn, &ensure_same_origin_and_csrf_token!(&1, csrf_token))
  end

  ## Verification

  defp get_csrf_from_session(conn) do
    csrf_token = get_session(conn, "_csrf_token")
    if csrf_token && byte_size(csrf_token) == @encoded_token_size do
      csrf_token
    end
  end

  defp verified_request?(conn, csrf_token) do
    conn.method in @unprotected_methods
      || valid_csrf_token?(csrf_token, conn.params["_csrf_token"])
      || valid_csrf_token?(csrf_token, List.first(get_req_header(conn, "x-csrf-token")))
      || skip_csrf_protection?(conn)
  end

  defp valid_csrf_token?(<<csrf_token::@encoded_token_size-binary>>,
                         <<user_token::@double_encoded_token_size-binary, mask::@encoded_token_size-binary>>) do
    case Base.decode64(user_token) do
      {:ok, user_token} -> Plug.Crypto.masked_compare(csrf_token, user_token, mask)
      :error -> false
    end
  end

  defp valid_csrf_token?(_csrf_token, _user_token), do: false

  ## Before send

  defp ensure_same_origin_and_csrf_token!(conn, csrf_token) do
    if cross_origin_js?(conn) do
      raise InvalidCrossOriginRequestError
    end

    ensure_csrf_token(conn, csrf_token)
  end

  defp cross_origin_js?(%Plug.Conn{method: "GET"} = conn),
    do: not skip_csrf_protection?(conn) and not xhr?(conn) and js_content_type?(conn)
  defp cross_origin_js?(%Plug.Conn{}),
    do: false

  defp js_content_type?(conn) do
    conn
    |> get_resp_header("content-type")
    |> Enum.any?(&String.starts_with?(&1, ~w(text/javascript application/javascript)))
  end

  defp xhr?(conn) do
    "XMLHttpRequest" in get_req_header(conn, "x-requested-with")
  end

  defp ensure_csrf_token(conn, csrf_token) do
    Process.delete(:plug_masked_csrf_token)
    case Process.delete(:plug_unmasked_csrf_token) do
      ^csrf_token -> conn
      current     -> put_session(conn, "_csrf_token", current)
    end
  end

  ## Helpers

  defp skip_csrf_protection?(%Plug.Conn{private: %{plug_skip_csrf_protection: true}}), do: true
  defp skip_csrf_protection?(%Plug.Conn{}), do: false

  defp mask(token) do
    mask = generate_token()
    Base.encode64(Plug.Crypto.mask(token, mask)) <> mask
  end

  defp unmasked_csrf_token do
    if token = Process.get(:plug_unmasked_csrf_token) do
      token
    else
      token = generate_token()
      Process.put(:plug_unmasked_csrf_token, token)
      token
    end
  end

  defp generate_token do
    Base.encode64(:crypto.strong_rand_bytes(@token_size))
  end
end
