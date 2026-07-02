module Benchmark
  module Actions
    module Error
      class Get < Benchmark::Action
        def handle(request, response)
          DEBUG_MODE = ENV.fetch('DEBUG', 'false') == 'true'
          if DEBUG_MODE
            response.body = "Internal Server Error"
          else
            response.body = ""
          end
          response.status = 500
        end
      end
    end
  end
end