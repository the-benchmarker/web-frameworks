require 'hanami'

module Benchmark
  class App < Hanami::App
    config.logger.stream = File::NULL
  end
end
