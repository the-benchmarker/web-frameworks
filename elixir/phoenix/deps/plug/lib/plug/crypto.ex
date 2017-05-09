defmodule Plug.Crypto do
  @moduledoc """
  Namespace and module for crypto functionality.
  """

  use Bitwise

  @doc """
  A restricted version a `:erlang.binary_to_term/1` that
  forbids possibly unsafe terms.
  """
  def safe_binary_to_term(binary) when is_binary(binary) do
    safe_terms(:erlang.binary_to_term(binary))
  end

  defp safe_terms(list) when is_list(list) do
    safe_list(list)
    list
  end
  defp safe_terms(tuple) when is_tuple(tuple) do
    safe_tuple(tuple, tuple_size(tuple))
    tuple
  end
  defp safe_terms(map) when is_map(map) do
    :maps.fold(fn key, value, acc ->
      safe_terms(key)
      safe_terms(value)
      acc
    end, map, map)
  end
  defp safe_terms(other) when is_atom(other) or is_number(other) or is_bitstring(other) or
                              is_pid(other) or is_reference(other) do
    other
  end
  defp safe_terms(other) do
    raise ArgumentError, "cannot deserialize #{inspect other}, the term is not safe for deserialization"
  end

  defp safe_list([]), do: :ok
  defp safe_list([h | t]) when is_list(t) do
    safe_terms(h)
    safe_list(t)
  end
  defp safe_list([h | t]) do
    safe_terms(h)
    safe_terms(t)
  end

  defp safe_tuple(_tuple, 0), do: :ok
  defp safe_tuple(tuple, n) do
    safe_terms(:erlang.element(n, tuple))
    safe_tuple(tuple, n - 1)
  end

  @doc """
  Masks the token on the left with the token on the right.

  Both tokens are required to have the same size.
  """
  def mask(left, right) do
    mask(left, right, "")
  end

  defp mask(<<x, left::binary>>, <<y, right::binary>>, acc) do
    mask(left, right, <<acc::binary, x ^^^ y>>)
  end

  defp mask(<<>>, <<>>, acc) do
    acc
  end

  @doc """
  Compares the two binaries (one being masked) in constant-time to avoid
  timing attacks.

  It is assumed the right token is masked according to the given mask.
  """
  def masked_compare(left, right, mask) do
    if byte_size(left) == byte_size(right) do
      masked_compare(left, right, mask, 0) == 0
    else
      false
    end
  end

  defp masked_compare(<<x, left::binary>>, <<y, right::binary>>, <<z, mask::binary>>, acc) do
    masked_compare(left, right, mask, acc ||| (x ^^^ (y ^^^ z)))
  end

  defp masked_compare(<<>>, <<>>, <<>>, acc) do
    acc
  end

  @doc """
  Compares the two binaries in constant-time to avoid timing attacks.

  See: http://codahale.com/a-lesson-in-timing-attacks/
  """
  def secure_compare(left, right) do
    if byte_size(left) == byte_size(right) do
      secure_compare(left, right, 0) == 0
    else
      false
    end
  end

  defp secure_compare(<<x, left :: binary>>, <<y, right :: binary>>, acc) do
    secure_compare(left, right, acc ||| (x ^^^ y))
  end

  defp secure_compare(<<>>, <<>>, acc) do
    acc
  end
end
