require "option_parser"
require "./config"

# Server defaults
port = 3000
host = "127.0.0.1"
cluster = false
process_count = 1

# Command line options
OptionParser.parse! do |parser|
  parser.banner = "Usage: #{PROGRAM_NAME} [arguments]"

  parser.on("-b HOST", "--bind=HOST", "Specifies the server host") { |h| host = h }
  parser.on("-p PORT", "--port=PORT", "Specifies the server port") { |p| port = p.to_i }

  parser.on("-c COUNT", "--cluster=COUNT", "Specifies the number of processes to handle requests") do |c|
    cluster = true
    process_count = c.to_i
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

# Detect ctr-c to shutdown gracefully
Signal::INT.trap do |signal|
  if cluster
    puts " > terminating cluster"
    signal.ignore
    spawn { server.close }
  else
    puts " > terminating gracefully"
    server.close
  end
end

# Start clustering
server.cluster(process_count) if cluster

# Start the server
server.run do
  puts "Listening on #{server.print_addresses}"
end

# Shutdown message
puts "#{APP_NAME} leaps through the veldt\n"
