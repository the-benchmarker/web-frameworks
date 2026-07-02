module Benchmark
  module Actions
    module Health
      class Get < Benchmark::Action
        def handle(request, response)
          response.body = "OK"
        end
      end
    end
  end
end