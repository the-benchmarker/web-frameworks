# frozen_string_literal: true

require 'camping'

Camping.goes :App

module App
  module Controllers
    class Index < R '/'
      def get
        ''
      end
    end

    class User < R '/user/(\d+)'
      def get(id)
        id
      end
    end

    class Creator < R '/user/(\d+)'
      def post
        ''
      end
    end
  end
end
