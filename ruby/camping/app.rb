# frozen_string_literal: true

Camping.goes :App

module App::Controllers
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

  class Creator < R '/user'
    def post
      ''
    end
  end
end
