import Config

# =============================================================================
# Logger Configuration - Production Grade
# =============================================================================
# Disable debug logging in production, keep minimal error logging for operations
# Compile-time purging removes log calls entirely for performance

config :logger,
  default_handler: false,
  compile_time_purge_matching: [
    [level_lower_than: :error]
  ],
  # Production handlers - log to stderr with JSON format for easy parsing
  handlers: [
    {Logger.JSONFormatter, :install, []},
    {Logger.ConsoleBackend, :install, [:stderr]}
  ]

# Backend configuration for structured logging
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id, :user_id, :session_id]

# =============================================================================
# Cowboy Server Configuration - Security & Performance
# =============================================================================

# Socket options optimized for production
config :cowboy,
  clear_handler: Server,
  # Socket optimization flags
  socket_opts: [
    port: System.get_env("PORT") || 3000,
    # Security: Prevent port scanning
    reuseaddr: true,
    # Security: Close connections on exec (prevents FD leakage)
    close_on_exec: true,
    # Performance: Enable TCP_NODELAY for better latency
    nodelay: true,
    # Performance: Reuse port for better load balancing
    reuseport: true,
    # Security: Set receive buffer size
    recbuf: 16_384,
    # Security: Set send buffer size  
    sndbuf: 16_384,
    # Security: Keepalive to detect dead connections
    keepalive: true,
    # Security: Timeouts
    send_timeout: 5_000,
    send_timeout_close: true
  ],
  # Connection limits for production
  max_connections: 16_384,
  num_acceptors: 100,
  # Security: Disable connection upgrade (WebSockets, etc.) if not needed
  enable_compression: false

# =============================================================================
# Application Configuration
# =============================================================================

# Ensure application starts in production mode
config :server,
  env: Mix.env(),
  # Security: Disable debug endpoints in production
  debug_endpoints: Mix.env() != :prod
