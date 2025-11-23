module Benchmark
  module Actions
    module Home
      class Index < Benchmark::Action
        def handle(request, response)
          response.body = ""
        end
      end
    end
  end
end
