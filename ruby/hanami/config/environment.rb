# frozen_string_literal: true

require("bundler/setup")
require("hanami/setup")
require_relative("../apps/web/application")

Hanami.configure do
  mount Web::Application, at: "/"
end
