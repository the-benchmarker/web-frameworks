defmodule Plug.Crypto.MessageVerifier do
  @moduledoc """
  `MessageVerifier` makes it easy to generate and verify messages
  which are signed to prevent tampering.

  For example, the cookie store uses this verifier to send data
  to the client. The data can be read by the client, but cannot be
  tampered with.
  """

  @doc """
  Signs a message according to the given secret.
  """
  def sign(message, secret, digest_type \\ :sha256)
  def sign(message, secret, digest_type)
      when is_binary(message) and is_binary(secret) and digest_type in [:sha256, :sha384, :sha512] do
    hmac_sha2_sign(message, secret, digest_type)
  end
  def sign(message, secret, :sha)
      when is_binary(message) and is_binary(secret) do
    hmac_sha1_sign(message, secret)
  end

  @doc """
  Decodes and verifies the encoded binary was not tampered with.
  """
  def verify(signed, secret)
      when is_binary(signed) and is_binary(secret) do
    if String.contains?(signed, ".") do
      hmac_sha2_verify(signed, secret)
    else
      hmac_sha1_verify(signed, secret)
    end
  end

  ## Signature Algorithms

  defp hmac_sha1_sign(payload, key)
      when is_binary(payload) and is_binary(key) do
    plain_text = Base.url_encode64(payload)
    signature  = :crypto.hmac(:sha, key, plain_text)
    plain_text <> "##" <> Base.url_encode64(signature)
  end

  defp hmac_sha1_verify(signed, key)
      when is_binary(signed) and is_binary(key) do
    case decode_legacy_token(signed) do
      {"HS1", payload, plain_text, signature} ->
        challenge = :crypto.hmac(:sha, key, plain_text)
        if Plug.Crypto.secure_compare(challenge, signature) do
          {:ok, payload}
        else
          :error
        end
      _ ->
        :error
    end
  end

  defp hmac_sha2_to_protected(:sha256), do: "HS256"
  defp hmac_sha2_to_protected(:sha384), do: "HS384"
  defp hmac_sha2_to_protected(:sha512), do: "HS512"

  defp hmac_sha2_to_digest_type("HS256"), do: :sha256
  defp hmac_sha2_to_digest_type("HS384"), do: :sha384
  defp hmac_sha2_to_digest_type("HS512"), do: :sha512

  defp hmac_sha2_sign(payload, key, digest_type) do
    protected  = hmac_sha2_to_protected(digest_type)
    plain_text = signing_input(protected, payload)
    signature  = :crypto.hmac(digest_type, key, plain_text)
    encode_token(plain_text, signature)
  end

  defp hmac_sha2_verify(signed, key)
      when is_binary(signed) and is_binary(key) do
    case decode_token(signed) do
      {protected, payload, plain_text, signature} when protected in ["HS256", "HS384", "HS512"] ->
        digest_type = hmac_sha2_to_digest_type(protected)
        challenge = :crypto.hmac(digest_type, key, plain_text)
        if Plug.Crypto.secure_compare(challenge, signature) do
          {:ok, payload}
        else
          :error
        end
      _ ->
        :error
    end
  end

  ## Helpers

  defp encode_token(plain_text, signature)
      when is_binary(plain_text) and is_binary(signature) do
    plain_text <> "." <> Base.url_encode64(signature, padding: false)
  end

  defp decode_token(token) do
    case String.split(token, ".", parts: 3) do
      [protected, payload, signature] ->
        plain_text = protected <> "." <> payload
        # TODO: Use with/else once we depend on Elixir v1.3+ only
        with {:ok, protected} <- Base.url_decode64(protected, padding: false),
             {:ok, payload}   <- Base.url_decode64(payload, padding: false),
             {:ok, signature} <- Base.url_decode64(signature, padding: false) do
          {true, protected, payload, plain_text, signature}
        end
        |> case do
          {true, protected, payload, plain_text, signature} ->
            {protected, payload, plain_text, signature}
          _ ->
            :error
        end
      _ ->
        :error
    end
  end

  defp signing_input(protected, payload)
      when is_binary(protected) and is_binary(payload) do
    protected
    |> Base.url_encode64(padding: false)
    |> Kernel.<>(".")
    |> Kernel.<>(Base.url_encode64(payload, padding: false))
  end

  ## Legacy Helpers

  defp decode_legacy_token(token) do
    token
    |> String.split("##", parts: 2)
    |> case do
      [_, _] = both -> both
      _ -> String.split(token, "--", parts: 2)
    end
    |> case do
      [plain_text, signature] when byte_size(plain_text) > 0 and byte_size(signature) > 0 ->
        # TODO: Use with/else once we depend on Elixir v1.3+ only
        with {:ok, payload}   <- decode_legacy_base64(plain_text),
             {:ok, signature} <- decode_legacy_base64(signature) do
          {true, "HS1", payload, plain_text, signature}
        end
        |> case do
          {true, protected, payload, plain_text, signature} ->
            {protected, payload, plain_text, signature}
          _ ->
            :error
        end
      _ ->
        :error
    end
  end

  # TODO: Remove after backwards compatibility period
  defp decode_legacy_base64(content) do
    case Base.url_decode64(content) do
      {:ok, binary} -> {:ok, binary}
      :error -> Base.decode64(content)
    end
  end
end
