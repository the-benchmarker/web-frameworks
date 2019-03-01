# frozen_string_literal: true

class App
  class << self
    def call(env)
      process env
    rescue StandardError => e
      handle_error e
    end

    def process(env)
      request = Request.new(env)
      request.response.finish
    end

    def handle_error(error)
      puts "Error processing request: #{error.message}"
      puts error.backtrace[0..6]

      Rack::Response.new('Error', 500).finish
    end
  end
end
