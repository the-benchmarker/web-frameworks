require "option_parser"
require "./config"

# Server defaults
port = 3000
host = "127.0.0.1"

# Command line options
OptionParser.parse! do |parser|
  parser.banner = "Usage: #{PROGRAM_NAME} [arguments]"

  parser.on("-b HOST", "--bind=HOST", "Specifies the server host") { |h| host = h }
  parser.on("-p PORT", "--port=PORT", "Specifies the server port") { |p| port = p.to_i }

  parser.on("-r", "--routes", "List the application routes") do
    ActionController::Server.print_routes
    exit 0
  end

  parser.on("-v", "--version", "Display the application version") do
    puts "#{APP_NAME} v#{VERSION}"
    exit 0
  end

  parser.on("-h", "--help", "Show this help") do
    puts parser
    exit 0
  end
end

# Load the routes
puts "Launching #{APP_NAME} v#{VERSION}"
server = ActionController::Server.new(port, host)

# Detect ctr-c to shutdown gracefully
Signal::INT.trap do
  puts " > terminating gracefully"
  server.close
end

# Start the server
puts "Listening on tcp://#{host}:#{port}"
server.run

# Shutdown message
puts "#{APP_NAME} leaps through the veldt\n"
