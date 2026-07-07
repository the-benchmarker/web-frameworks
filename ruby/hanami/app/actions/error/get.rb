module Benchmark
  module Actions
    module Error
      class Get < Benchmark::Action
        def handle(request, response)
          response.body = DEBUG_MODE ? "Internal Server Error" : ""
          response.status = 500
        end
      end
    end
  end
end