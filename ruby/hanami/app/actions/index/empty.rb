module Benchmark
  module Actions
    module Index
      class Empty < Benchmark::Action
        def handle(request, response)
          response.body = ""
        end
      end
    end
  end
end
