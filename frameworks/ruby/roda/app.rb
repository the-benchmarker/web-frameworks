require "roda"

class App < Roda
  route do |r|
    r.get "user", Integer do |id|
      id.to_s
    end

    r.post("user") do
      ""
    end

    r.root do
      ""
    end
  end
end
