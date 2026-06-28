# Production-grade Supranim web server
# Security best practices, performance optimizations, and clean code
# https://github.com/supranim/supranim

import std/[httpcore, macros, macrocache, options]

import pkg/kapsis/[framework, runtime]
import pkg/supranim
import pkg/supranim/controller
import pkg/supranim/core/[request, router, response]

include ./routes

import ./controller/[pages, errors]

## Application constants
const APP_NAME = "Supranim Benchmark Server"
const APP_VERSION = "1.0.0"

## Initialize application with production settings
proc initializeApp() =
  # Framework initialization
  initApplication()
  initHttpRouter()
  
  # Production configuration
  App.configs = newOrderedTable[string, YAMLObject]()
  
  # Server configuration - production settings
  App.configs["server"] = parseYAML("""
    port: 3000
    host: 0.0.0.0
    debug: false
    log_level: error
    max_threads: 100
    max_connections: 10000
  """)
  
  # Security configuration
  App.configs["security"] = parseYAML("""
    secure_cookies: true
    http_only_cookies: true
    same_site_cookies: lax
    content_security_policy: default-src 'self'
    x_frame_options: DENY
    x_content_type_options: nosniff
    x_xss_protection: 1; mode=block
  """)
  
  # Performance configuration
  App.configs["performance"] = parseYAML("""
    compression: true
    cache_control: max-age=3600
    read_buffer_size: 8192
    write_buffer_size: 8192
  """)

# Start server in production mode
when isMainModule:
  echo "Starting ", APP_NAME, " v", APP_VERSION, " on port 3000"
  initializeApp()
  App.run()