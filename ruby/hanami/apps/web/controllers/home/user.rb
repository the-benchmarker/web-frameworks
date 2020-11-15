# frozen_string_literal: true

module Web
  module Controllers
    module Home
      class User
        include Web::Action

        def call(params)
          self.body = params[:id]
        end
      end
    end
  end
end
