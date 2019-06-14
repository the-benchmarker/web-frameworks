# frozen_string_literal: true

module FlameTest
  # Application that will be launched
  class Application < Flame::Application
    mount IndexController, "/"
    mount UserController
  end
end
