# frozen_string_literal: true

require 'camping'

Camping.goes :Server

module Server
  module Controllers
    class Index
      def get
        ''
      end
    end

    class User
      def post
        ''
      end
    end

    class UserCreate < R '/user/(\d+)'
      def get(id)
        id
      end
    end
  end
end
