require "roda"

class App < Roda
  route do |r|
    r.get("user", ":id") { |id| id.to_s }
    r.post("user") { "" }
    r.root { "" }
  end
end
