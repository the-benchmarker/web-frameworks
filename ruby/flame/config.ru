# frozen_string_literal: true

require 'rubygems' unless defined? ::Gem

require 'flame'

Flame::Application.require_dirs(
  %w[controllers]
)

require './application'

run FlameTest::Application
