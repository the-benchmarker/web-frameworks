require "option_parser"
require "./config"

# Server defaults
port = (ENV["SG_SERVER_PORT"]? || 3000).to_i
host = ENV["SG_SERVER_HOST"]? || "0.0.0.0"
thread_count = System.cpu_count.to_i

# Command line options
OptionParser.parse(ARGV.dup) do |parser|
  parser.banner = "Usage: #{PROGRAM_NAME} [arguments]"

  parser.on("-b HOST", "--bind=HOST", "Specifies the server host") { |h| host = h }
  parser.on("-p PORT", "--port=PORT", "Specifies the server port") { |p| port = p.to_i }

  parser.on("-w COUNT", "--workers=COUNT", "Specifies the number of threads to handle requests") do |w|
    thread_count = w.to_i
  end

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

# number of threads to spawn
server.threads(thread_count)

# Handle termination signal
Process.on_terminate do
  puts "\n > terminating gracefully"
  server.close
end

# Start the server
server.run do
  puts "Listening on #{server.print_addresses}"
end

# Shutdown message
puts "#{APP_NAME} leaps through the veldt\n"
