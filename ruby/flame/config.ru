# frozen_string_literal: true

require("flame")

Dir["controllers/**/*"].each { |controller| require_relative controller }

require_relative("application")

run(FlameTest::Application)
