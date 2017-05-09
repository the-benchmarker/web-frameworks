defmodule Plug.Session.COOKIE do
  @moduledoc """
  Stores the session in a cookie.

  This cookie store is based on `Plug.Crypto.MessageVerifier`
  and `Plug.Crypto.Message.Encryptor` which encrypts and signs
  each cookie to ensure they can't be read nor tampered with.

  Since this store uses crypto features, it requires you to
  set the `:secret_key_base` field in your connection. This
  can be easily achieved with a plug:

      plug :put_secret_key_base

      def put_secret_key_base(conn, _) do
        put_in conn.secret_key_base, "-- LONG STRING WITH AT LEAST 64 BYTES --"
      end

  ## Options

    * `:encryption_salt` - a salt used with `conn.secret_key_base` to generate
      a key for encrypting/decrypting a cookie.

    * `:signing_salt` - a salt used with `conn.secret_key_base` to generate a
      key for signing/verifying a cookie;

    * `:key_iterations` - option passed to `Plug.Crypto.KeyGenerator`
      when generating the encryption and signing keys. Defaults to 1000;

    * `:key_length` - option passed to `Plug.Crypto.KeyGenerator`
      when generating the encryption and signing keys. Defaults to 32;

    * `:key_digest` - option passed to `Plug.Crypto.KeyGenerator`
      when generating the encryption and signing keys. Defaults to `:sha256`;

    * `:serializer` - cookie serializer module that defines `encode/1` and
      `decode/1` returning an `{:ok, value}` tuple. Defaults to
      `:external_term_format`.

    * `:log` - Log level to use when the cookie cannot be decoded.
      Defaults to `:debug`, can be set to false to disable it.

  ## Examples

      # Use the session plug with the table name
      plug Plug.Session, store: :cookie,
                         key: "_my_app_session",
                         encryption_salt: "cookie store encryption salt",
                         signing_salt: "cookie store signing salt",
                         key_length: 64,
                         log: :debug
  """

  require Logger
  @behaviour Plug.Session.Store

  alias Plug.Crypto.KeyGenerator
  alias Plug.Crypto.MessageVerifier
  alias Plug.Crypto.MessageEncryptor

  def init(opts) do
    encryption_salt = opts[:encryption_salt]
    signing_salt = check_signing_salt(opts)

    iterations = Keyword.get(opts, :key_iterations, 1000)
    length = Keyword.get(opts, :key_length, 32)
    digest = Keyword.get(opts, :key_digest, :sha256)
    log = Keyword.get(opts, :log, :debug)
    key_opts = [iterations: iterations,
                length: length,
                digest: digest,
                cache: Plug.Keys]

    serializer = check_serializer(opts[:serializer] || :external_term_format)

    %{encryption_salt: encryption_salt,
      signing_salt: signing_salt,
      key_opts: key_opts,
      serializer: serializer,
      log: log}
  end

  def get(conn, cookie, opts) do
    %{key_opts: key_opts, signing_salt: signing_salt, log: log} = opts

    case opts do
      %{encryption_salt: nil} ->
        MessageVerifier.verify(cookie, derive(conn, signing_salt, key_opts))
      %{encryption_salt: key} ->
        # TODO: Change to verify/3 after backwards compatibility period.
        MessageEncryptor.verify_and_decrypt(cookie,
                                            derive(conn, key, key_opts),
                                            derive(conn, signing_salt, key_opts))
    end |> decode(opts.serializer, log)
  end

  def put(conn, _sid, term, opts) do
    %{serializer: serializer, key_opts: key_opts, signing_salt: signing_salt} = opts
    binary = encode(term, serializer)

    case opts do
      %{encryption_salt: nil} ->
        MessageVerifier.sign(binary, derive(conn, signing_salt, key_opts))
      %{encryption_salt: key} ->
        MessageEncryptor.encrypt(binary,
                                 derive(conn, key, key_opts),
                                 derive(conn, signing_salt, key_opts))
    end
  end

  def delete(_conn, _sid, _opts) do
    :ok
  end

  defp encode(term, :external_term_format) do
    :erlang.term_to_binary(term)
  end

  defp encode(term, serializer) do
    {:ok, binary} = serializer.encode(term)
    binary
  end

  defp decode({:ok, binary}, :external_term_format, log) do
    {:term,
      try do
        Plug.Crypto.safe_binary_to_term(binary)
      rescue
        e ->
          Logger.log log, "Plug.Session could not decode incoming session cookie. Reason: " <>
                          Exception.message(e)
          %{}
      end}
  end

  defp decode({:ok, binary}, serializer, _log) do
    case serializer.decode(binary) do
      {:ok, term} -> {:custom, term}
      _           -> {:custom, %{}}
    end
  end

  defp decode(:error, _serializer, false) do
    {nil, %{}}
  end

  defp decode(:error, _serializer, log) do
    Logger.log log, "Plug.Session could not verify incoming session cookie. " <>
                    "This may happen when the session settings change or a stale cookie is sent."
    {nil, %{}}
  end

  defp derive(conn, key, key_opts) do
    conn.secret_key_base
    |> validate_secret_key_base()
    |> KeyGenerator.generate(key, key_opts)
  end

  defp validate_secret_key_base(nil), do:
    raise(ArgumentError, "cookie store expects conn.secret_key_base to be set")
  defp validate_secret_key_base(secret_key_base) when byte_size(secret_key_base) < 64, do:
    raise(ArgumentError, "cookie store expects conn.secret_key_base to be at least 64 bytes")
  defp validate_secret_key_base(secret_key_base), do:
    secret_key_base

  defp check_signing_salt(opts) do
    case opts[:signing_salt] do
      nil  -> raise ArgumentError, "cookie store expects :signing_salt as option"
      salt -> salt
    end
  end

  defp check_serializer(serializer) when is_atom(serializer), do: serializer
  defp check_serializer(_), do:
    raise(ArgumentError, "cookie store expects :serializer option to be a module")
end
