import Config

config :server, Server.Endpoint,
  url: [host: "localhost"],
  http: [port: 3000],
  server: true

config :logger,
  default_handler: false,
  compile_time_purge_matching: [
    [level_lower_than: :emergency]
  ]

config :phoenix, :json_library, Jason
