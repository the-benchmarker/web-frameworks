# frozen_string_literal: true

Pakyow.app :pakyow, only: [:routing] do
  # Learn more about configuring an application:
  #
  #   * https://pakyow.com/docs/configuration
  #
  configure do
    # Application config for all environments.
  end

  configure :development do
    # Application config for the development environment.
  end

  configure :production do
    # Application config for the production environment.
  end
end
