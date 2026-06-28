#!/usr/bin/env crystal
# Grip Framework Production Server
# Optimized for production deployments with security best practices

require "grip"
require "log"

# Logging: Completely disable logging for production performance
Log.setup(:none)

# Index Controller
class IndexController
  include Grip::Controllers::HTTP

  def get(context : Context) : Context
    # Health check endpoint
    if context.request.path == "/health"
      return context.put_status(200).text("OK").halt()
    end
    
    # Root endpoint
    context.put_status(200).text("").halt()
  end
end

# User Controller
class UserController
  include Grip::Controllers::HTTP

  def get(context : Context) : Context
    id = context.fetch_path_params.["id"]
    
    # Input validation
    if id.empty?
      return context.put_status(400).text("Invalid user ID").halt()
    end
    
    context.text(id).halt()
  end
  
  def post(context : Context): Context
    context.put_status(201).text("").halt()
  end
end

# Application Configuration
class Application
  include Grip::Application

  property handlers : Array(HTTP::Handler) = [
    Grip::Handlers::HTTP.new
  ] of HTTP::Handler

  def initialize
    # Health check endpoint
    get "/health", IndexController
    
    # Root endpoint
    get "/", IndexController
    
    # User endpoints
    get "/user/:id", UserController
    post "/user", UserController
  end
end

# Production server startup with clustering support
port = ENV["PORT"]? ? ENV["PORT"].to_i : 3000
host = ENV["HOST"]? || "0.0.0.0"
worker_count = ENV["WORKER_COUNT"]? ? ENV["WORKER_COUNT"].to_i : System.cpu_count

app = Application.new
app.run(host: host, port: port, workers: worker_count)
