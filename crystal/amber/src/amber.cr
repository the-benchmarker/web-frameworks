require "../config/*"

Amber.env = "production"
Amber::Server.configure do |app|
  app.host = "0.0.0.0"
end
Amber::Server.start
