import Config

# Configures the endpoint
config :server, Server.Endpoint,
  url: [host: "localhost"],
  http: [port: 8080],
  server: true

config :logger, level: :warn

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason
