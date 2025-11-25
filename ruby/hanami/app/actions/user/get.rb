module Benchmark
  module Actions
    module User
      class Get < Benchmark::Action
        def handle(request, response)
          response.body = request.params[:id]
        end
      end
    end
  end
end
