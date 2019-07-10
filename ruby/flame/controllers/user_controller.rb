# frozen_string_literal: true

module FlameTest
  # Controller for `/user` paths
  class UserController < Flame::Controller
    def show(id)
      halt 404 unless id.match(/\A\d+\z/)
      id
    end

    def create
      # do nothing
    end
  end
end
