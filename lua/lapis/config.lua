-- Production-grade Lapis configuration
-- 
-- Best practices for production deployment:
-- - Disabled debug mode
-- - Disabled development logging
-- - Code caching enabled for performance
-- - Auto-scaling workers based on available cores
-- - Security-optimized nginx server configuration
-- - Production server settings

local config = require("lapis.config")

config("production", {
  -- Server configuration
  server = "nginx",
  bind_host = "0.0.0.0",
  port = 3000,
  
  -- Performance settings
  code_cache = "on",
  num_workers = "auto",
  
  -- Security and production settings
  logging = false,
  debug = false,
  
  -- LuaJIT optimization flags for production
  luajit = {
    jit_flush = true,
    jit_p = "v",
    jit_allfuncs = true,
  },
  
  -- nginx-specific production settings
  nginx = {
    -- Disable access log for benchmarking performance
    access_log = "off",
    -- Enable keepalive connections for performance
    keepalive_timeout = "75s",
    keepalive_requests = 1000,
    -- Buffer settings for optimal performance
    client_body_buffer_size = "16k",
    client_header_buffer_size = "2k",
    large_client_header_buffers = "4 8k",
    -- Security headers at server level
    add_headers = {
      ["X-Content-Type-Options"] = "nosniff",
      ["X-Frame-Options"] = "DENY",
      ["X-XSS-Protection"] = "1; mode=block",
    },
    -- Hide nginx version for security
    server_tokens = "off",
    -- Timeouts for production stability
    client_body_timeout = "60s",
    client_header_timeout = "60s",
    send_timeout = "60s",
  },
  
  -- Lua socket configuration
  lua = {
    -- Disable socket debug
    socket_debug = false,
    -- Connection pool settings
    pool_timeout = 60,
    pool_size = 100,
  },
})
