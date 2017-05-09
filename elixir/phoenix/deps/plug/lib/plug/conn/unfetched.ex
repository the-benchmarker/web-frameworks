defmodule Plug.Conn.Unfetched do
  @moduledoc """
  A struct used as default on unfetched fields.

  The `:aspect` key of the struct specifies what field is still unfetched.

  ## Examples

      unfetched = %Plug.Conn.Unfetched{aspect: :cookies}

  """

  defstruct [:aspect]
  @type t :: %__MODULE__{aspect: atom()}

  @doc false
  def get(%{aspect: aspect}, key, _value) do
    raise ArgumentError, "cannot get key #{inspect key} from conn.#{aspect} because they were not fetched"
  end

  @doc false
  def get_and_update(%{aspect: aspect}, key, _fun) do
    raise ArgumentError, "cannot get_and_update key #{inspect key} from conn.#{aspect} because they were not fetched"
  end
end
