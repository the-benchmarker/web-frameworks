require "log"

# About logger.cr File
#
# Amber is using the crystal standard library Log
# You can read details here: https://crystal-lang.org/api/0.34.0/Log.html

# Using environment settings:
backend = Log::IOBackend.new(STDOUT)

# Custom formatter
# This is a good place to change the time from UTC

# if you want the systems local time or hard code a timezone, uncomment
# one of the following lines and update the formatter accordingly
# time_zone = Time::Location.local
# time_zone = Time::Location.load("America/Buenos_Aires")

backend.formatter = Log::Formatter.new do |entry, io|
  io << entry.timestamp.to_s("%I:%M:%S")
  # io << entry.timestamp.in(time_zone).to_s("%I:%M:%S")
  io << " "
  io << entry.source
  io << " (#{entry.severity})" if entry.severity > Log::Severity::Debug
  io << " "
  io << entry.message
end

Log.builder.bind "*", Amber.settings.logging.severity, backend

# Using crystal's standard environment variables:
# CRYSTAL_LOG_LEVEL=INFO
# CRYSTAL_LOG_SOURCES=*
# Logs are emitted to STDOUT
# Log.setup_from_env

# Using more advanced options:
# backend = Log::IOBackend.new
# Log.builder.bind "*", :warning, backend
# Log.builder.bind "request", :debug, backend
# Log.builder.bind "headers", :debug, backend
# Log.builder.bind "cookies", :debug, backend
# Log.builder.bind "params", :debug, backend
# Log.builder.bind "session", :debug, backend
# Log.builder.bind "errors", :warning, backend
# Log.builder.bind "granite.*", :info, backend
# Log.builder.bind "*", :error, ElasticSearchBackend.new("http://localhost:9200")