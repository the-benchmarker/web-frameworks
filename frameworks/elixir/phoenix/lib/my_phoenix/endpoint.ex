defmodule MyPhoenix.Endpoint do
  use Phoenix.Endpoint, otp_app: :my_phoenix

  plug MyPhoenix.Router
end
