# frozen_string_literal: true

require("roda")

class App < Roda
  route do |r|
    r.root do
      ""
    end

    r.on("user") do
      r.get(String) do |id|
        id
      end

      r.post(true) do
        ""
      end
    end
  end
end
