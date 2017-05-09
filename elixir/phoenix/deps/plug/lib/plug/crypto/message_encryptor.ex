defmodule Plug.Crypto.MessageEncryptor do
  @moduledoc ~S"""
  `MessageEncryptor` is a simple way to encrypt values which get stored
  somewhere you don't trust.

  The encrypted key, initialization vector, cipher text, and cipher tag
  are base64url encoded and returned to you.

  This can be used in situations similar to the `MessageVerifier`, but where
  you don't want users to be able to determine the value of the payload.

  ## Example

      secret_key_base = "072d1e0157c008193fe48a670cce031faa4e..."
      encrypted_cookie_salt = "encrypted cookie"
      encrypted_signed_cookie_salt = "signed encrypted cookie"

      secret = KeyGenerator.generate(secret_key_base, encrypted_cookie_salt)
      sign_secret = KeyGenerator.generate(secret_key_base, encrypted_signed_cookie_salt)

      data = "José"
      encrypted = MessageEncryptor.encrypt(data, secret, sign_secret)
      decrypted = MessageEncryptor.decrypt(encrypted, secret, sign_secret)
      decrypted # => {:ok, "José"}
  """

  alias Plug.Crypto.MessageVerifier

  @doc """
  Encrypts a message using authenticated encryption.
  """
  def encrypt(message, secret, sign_secret)
      when is_binary(message) and is_binary(secret) and is_binary(sign_secret) do
    aes128_gcm_encrypt(message, secret, sign_secret)
  end

  @doc """
  Decrypts a message using authenticated encryption.
  """
  def decrypt(encrypted, secret, sign_secret)
      when is_binary(encrypted) and is_binary(secret) and is_binary(sign_secret) do
    aes128_gcm_decrypt(encrypted, secret, sign_secret)
  end

  # Encrypts and authenticates a message using AES128-GCM mode.
  #
  # A random 128-bit content encryption key (CEK) is generated for
  # every message which is then encrypted with `aes_gcm_key_wrap/3`.
  defp aes128_gcm_encrypt(plain_text, secret, sign_secret) when bit_size(secret) > 256 do
    aes128_gcm_encrypt(plain_text, binary_part(secret, 0, 32), sign_secret)
  end
  defp aes128_gcm_encrypt(plain_text, secret, sign_secret)
       when is_binary(plain_text) and bit_size(secret) in [128, 192, 256] and is_binary(sign_secret) do
    key = :crypto.strong_rand_bytes(16)
    iv = :crypto.strong_rand_bytes(12)
    aad = "A128GCM"
    {cipher_text, cipher_tag} = :crypto.block_encrypt(:aes_gcm, key, iv, {aad, plain_text})
    encrypted_key = aes_gcm_key_wrap(key, secret, sign_secret)
    encode_token(aad, encrypted_key, iv, cipher_text, cipher_tag)
  end

  # Verifies and decrypts a message using AES128-GCM mode.
  #
  # Decryption will never be performed prior to verification.
  #
  # The encrypted content encryption key (CEK) is decrypted
  # with `aes_gcm_key_unwrap/3`.
  defp aes128_gcm_decrypt(cipher_text, secret, sign_secret) when bit_size(secret) > 256 do
    aes128_gcm_decrypt(cipher_text, binary_part(secret, 0, 32), sign_secret)
  end
  defp aes128_gcm_decrypt(cipher_text, secret, sign_secret)
       when is_binary(cipher_text) and bit_size(secret) in [128, 192, 256] and is_binary(sign_secret) do
    case decode_token(cipher_text) do
      {aad = "A128GCM", encrypted_key, iv, cipher_text, cipher_tag} when bit_size(iv) === 96 and bit_size(cipher_tag) === 128 ->
        encrypted_key
        |> aes_gcm_key_unwrap(secret, sign_secret)
        |> case do
          {:ok, key} ->
            :crypto.block_decrypt(:aes_gcm, key, iv, {aad, cipher_text, cipher_tag})
          _ ->
            :error
        end
        |> case do
          plain_text when is_binary(plain_text) ->
            {:ok, plain_text}
          _ ->
            :error
        end
      _ ->
        :error
    end
  end

  # Wraps a decrypted content encryption key (CEK) with secret and
  # sign_secret using AES GCM mode.
  #
  # See: https://tools.ietf.org/html/rfc7518#section-4.7
  defp aes_gcm_key_wrap(cek, secret, sign_secret) when bit_size(secret) > 256 do
    aes_gcm_key_wrap(cek, binary_part(secret, 0, 32), sign_secret)
  end
  defp aes_gcm_key_wrap(cek, secret, sign_secret)
       when bit_size(cek) in [128, 192, 256] and bit_size(secret) in [128, 192, 256] and is_binary(sign_secret) do
    iv = :crypto.strong_rand_bytes(12)
    {cipher_text, cipher_tag} = :crypto.block_encrypt(:aes_gcm, secret, iv, {sign_secret, cek})
    cipher_text <> cipher_tag <> iv
  end

  # Unwraps an encrypted content encryption key (CEK) with secret and
  # sign_secret using AES GCM mode.
  #
  # See: https://tools.ietf.org/html/rfc7518#section-4.7
  defp aes_gcm_key_unwrap(wrapped_cek, secret, sign_secret) when bit_size(secret) > 256 do
    aes_gcm_key_unwrap(wrapped_cek, binary_part(secret, 0, 32), sign_secret)
  end
  defp aes_gcm_key_unwrap(wrapped_cek, secret, sign_secret)
       when bit_size(secret) in [128, 192, 256] and is_binary(sign_secret) do
    wrapped_cek
    |> case do
      <<cipher_text :: 128-bitstring, cipher_tag :: 128-bitstring, iv :: 96-bitstring>> ->
        :crypto.block_decrypt(:aes_gcm, secret, iv, {sign_secret, cipher_text, cipher_tag})
      <<cipher_text :: 192-bitstring, cipher_tag :: 128-bitstring, iv :: 96-bitstring>> ->
        :crypto.block_decrypt(:aes_gcm, secret, iv, {sign_secret, cipher_text, cipher_tag})
      <<cipher_text :: 256-bitstring, cipher_tag :: 128-bitstring, iv :: 96-bitstring>> ->
        :crypto.block_decrypt(:aes_gcm, secret, iv, {sign_secret, cipher_text, cipher_tag})
      _ ->
        :error
    end
    |> case do
      cek when bit_size(cek) in [128, 192, 256] ->
        {:ok, cek}
      _ ->
        :error
    end
  end

  # Pads a message using the PKCS #7 cryptographic message syntax.
  #
  # See: https://tools.ietf.org/html/rfc2315
  # See: `pkcs7_unpad/1`
  defp pkcs7_pad(message) do
    bytes_remaining = rem(byte_size(message), 16)
    padding_size = 16 - bytes_remaining
    message <> :binary.copy(<<padding_size>>, padding_size)
  end

  # Unpads a message using the PKCS #7 cryptographic message syntax.
  #
  # See: https://tools.ietf.org/html/rfc2315
  # See: `pkcs7_pad/1`
  defp pkcs7_unpad(<<>>) do
    :error
  end
  defp pkcs7_unpad(message) do
    padding_size = :binary.last(message)
    if padding_size <= 16 do
      message_size = byte_size(message)
      if binary_part(message, message_size, -padding_size) === :binary.copy(<<padding_size>>, padding_size) do
        {:ok, binary_part(message, 0, message_size - padding_size)}
      else
        :error
      end
    else
      :error
    end
  end

  defp encode_token(protected, encrypted_key, iv, cipher_text, cipher_tag) do
    Base.url_encode64(protected, padding: false)
    |> Kernel.<>(".")
    |> Kernel.<>(Base.url_encode64(encrypted_key, padding: false))
    |> Kernel.<>(".")
    |> Kernel.<>(Base.url_encode64(iv, padding: false))
    |> Kernel.<>(".")
    |> Kernel.<>(Base.url_encode64(cipher_text, padding: false))
    |> Kernel.<>(".")
    |> Kernel.<>(Base.url_encode64(cipher_tag, padding: false))
  end

  defp decode_token(token) do
    case String.split(token, ".", parts: 5) do
      [protected, encrypted_key, iv, cipher_text, cipher_tag] ->
        # TODO: Use with/else once we depend on Elixir v1.3+ only
        with {:ok, protected}     <- Base.url_decode64(protected, padding: false),
             {:ok, encrypted_key} <- Base.url_decode64(encrypted_key, padding: false),
             {:ok, iv}            <- Base.url_decode64(iv, padding: false),
             {:ok, cipher_text}   <- Base.url_decode64(cipher_text, padding: false),
             {:ok, cipher_tag}    <- Base.url_decode64(cipher_tag, padding: false) do
          {true, protected, encrypted_key, iv, cipher_text, cipher_tag}
        end
        |> case do
          {true, protected, encrypted_key, iv, cipher_text, cipher_tag} ->
            {protected, encrypted_key, iv, cipher_text, cipher_tag}
          _ ->
            :error
        end
      _ ->
        :error
    end
  end

  ## Deprecated API

  @doc """
  WARNING: This function is deprecated in favor of `encrypt/3`.
  Encrypts and signs a message.
  """
  def encrypt_and_sign(message, secret, sign_secret, cipher \\ nil)
      when is_binary(message) and is_binary(secret) and is_binary(sign_secret) do
    # TODO: Deprecate after backwards compatibility period
    # IO.puts :stderr, "warning: `Plug.Crypto.MessageEncryptor.encrypt_and_sign/4` is deprecated," <>
    #                  "please use `encrypt/3` instead\n" <> Exception.format_stacktrace
    case cipher do
      nil ->
        encrypt(message, secret, sign_secret)
      :aes_cbc256 ->
        aes256_cbc_hmac_sha1_encrypt(message, secret, sign_secret)
      _ ->
        iv = :crypto.strong_rand_bytes(16)

        message
        |> pkcs7_pad()
        |> encrypt_legacy(cipher, secret, iv)
        |> Base.encode64()
        |> Kernel.<>("--")
        |> Kernel.<>(Base.encode64(iv))
        |> MessageVerifier.sign(sign_secret)
    end
  end

  @doc """
  WARNING: This function is deprecated in favor of `decrypt/3`.
  Decrypts and verifies a message.

  We need to verify the message in order to avoid padding attacks.
  Reference: http://www.limited-entropy.com/padding-oracle-attacks
  """
  def verify_and_decrypt(encrypted, secret, sign_secret, cipher \\ nil)
      when is_binary(encrypted) and is_binary(secret) and is_binary(sign_secret) do
    # TODO: Deprecate after backwards compatibility period
    # IO.puts :stderr, "warning: `Plug.Crypto.MessageEncryptor.verify_and_decrypt/4` is deprecated," <>
    #                  "please use `decrypt/3` instead\n" <> Exception.format_stacktrace
    case cipher do
      nil ->
        if String.contains?(encrypted, ".") do
          decrypt(encrypted, secret, sign_secret)
        else
          verify_and_decrypt(encrypted, secret, sign_secret, :aes_cbc256)
        end
      :aes_cbc256 ->
        aes256_cbc_hmac_sha1_decrypt(encrypted, secret, sign_secret)
      _ ->
        case MessageVerifier.verify(encrypted, sign_secret) do
          {:ok, verified} ->
            [encrypted, iv] = String.split(verified, "--")
            case Base.decode64(encrypted) do
              {:ok, encrypted} ->
                case Base.decode64(iv) do
                  {:ok, iv} ->
                    encrypted |> decrypt_legacy(cipher, secret, iv) |> pkcs7_unpad
                  :error ->
                    :error
                end
              :error ->
                :error
            end
          :error ->
            :error
        end
    end
  end

  defp encode_legacy_token(sign_secret, iv, cipher_text) do
    cipher_text = Base.encode64(cipher_text) <> "--" <> Base.encode64(iv)
    cipher_text = Base.url_encode64(cipher_text)
    cipher_tag = Base.url_encode64(:crypto.hmac(:sha, sign_secret, cipher_text))
    cipher_text <> "##" <> cipher_tag
  end

  defp decode_legacy_token(token, sign_secret) do
    token
    |> String.split("##", parts: 2)
    |> case do
      [_, _] = both -> both
      _ -> String.split(token, "--", parts: 2)
    end
    |> case do
      [cipher_text, cipher_tag]
          when byte_size(cipher_text) > 0 and byte_size(cipher_tag) > 0 ->
        # TODO: Use with/else once we depend on Elixir v1.3+ only
        with {:ok, cipher_tag}  <- Base.url_decode64(cipher_tag),
             challenge           = :crypto.hmac(:sha, sign_secret, cipher_text),
             true               <- Plug.Crypto.secure_compare(challenge, cipher_tag),
             {:ok, cipher_text} <- Base.url_decode64(cipher_text),
             [cipher_text, iv]  <- String.split(cipher_text, "--", parts: 2),
             {:ok, cipher_text} <- Base.decode64(cipher_text),
             {:ok, iv}          <- Base.decode64(iv) do
          {true, "A256CBC-HS1", "", iv, cipher_text, cipher_tag}
        end
        |> case do
          {true, protected, encrypted_key, iv, cipher_text, cipher_tag} ->
            {protected, encrypted_key, iv, cipher_text, cipher_tag}
          _ ->
            :error
        end
      _ ->
        :error
    end
  end

  defp encrypt_legacy(message, cipher, secret, iv)
      when bit_size(secret) > 256,
    do: encrypt_legacy(message, cipher, binary_part(secret, 0, 32), iv)
  defp encrypt_legacy(message, cipher, secret, iv) do
    :crypto.block_encrypt(cipher, secret, iv, message)
  end

  defp decrypt_legacy(encrypted, cipher, secret, iv)
      when bit_size(secret) > 256,
    do: decrypt_legacy(encrypted, cipher, binary_part(secret, 0, 32), iv)
  defp decrypt_legacy(encrypted, cipher, secret, iv) do
    :crypto.block_decrypt(cipher, secret, iv, encrypted)
  end

  # Encrypts and authenticates a message using AES128-CBC mode
  # with HMAC-SHA-1 for the authentication code.
  defp aes256_cbc_hmac_sha1_encrypt(plain_text, secret, sign_secret) when bit_size(secret) > 256 do
    aes256_cbc_hmac_sha1_encrypt(plain_text, binary_part(secret, 0, 32), sign_secret)
  end
  defp aes256_cbc_hmac_sha1_encrypt(plain_text, secret, sign_secret)
       when is_binary(plain_text) and bit_size(secret) in [128, 192, 256] and is_binary(sign_secret) do
    iv = :crypto.strong_rand_bytes(16)
    cipher_text = :crypto.block_encrypt(:aes_cbc256, secret, iv, pkcs7_pad(plain_text))
    encode_legacy_token(sign_secret, iv, cipher_text)
  end

  # Verifies and decrypts a message using AES128-CBC mode
  # with HMAC-SHA-1 for the authentication code.
  #
  # Decryption will never be performed prior to verification.
  defp aes256_cbc_hmac_sha1_decrypt(cipher_text, secret, sign_secret) when bit_size(secret) > 256 do
    aes256_cbc_hmac_sha1_decrypt(cipher_text, binary_part(secret, 0, 32), sign_secret)
  end
  defp aes256_cbc_hmac_sha1_decrypt(cipher_text, secret, sign_secret)
       when is_binary(cipher_text) and bit_size(secret) === 256 and is_binary(sign_secret) do
    case decode_legacy_token(cipher_text, sign_secret) do
      {"A256CBC-HS1", _encrypted_key, iv, cipher_text, cipher_tag}
          when bit_size(iv) === 128 and bit_size(cipher_tag) === 160 ->
        key = secret
        :crypto.block_decrypt(:aes_cbc256, key, iv, cipher_text)
        |> case do
          plain_text when is_binary(plain_text) ->
            pkcs7_unpad(plain_text)
          _ ->
            :error
        end
      _ ->
        :error
    end
  end
end
