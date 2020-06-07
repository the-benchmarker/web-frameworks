# frozen_string_literal: true

require("rubygems") unless defined? ::Gem
require(File.dirname(__FILE__) + "/app")

set(:logging, false)
set(:environment, :production)

run(Sinatra::Application)
