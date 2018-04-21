# frozen_string_literal: true

module FlameTest
  # Application that will be launched
  class Application < Flame::Application
    mount :index, '/'
    mount :user
  end
end
