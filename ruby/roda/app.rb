# frozen_string_literal: true

require "roda"

class App < Roda
  route do |r|
    r.get "user", Integer, &:to_s

    r.post("user") do
      ""
    end

    r.root do
      ""
    end
  end
end
