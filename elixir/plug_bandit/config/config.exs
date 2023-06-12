import Config

config :logger,
  default_handler: false,
  compile_time_purge_matching: [
    [level_lower_than: :emergency]
  ]
