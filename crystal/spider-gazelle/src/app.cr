require "option_parser"
require "./config"

# Server defaults
port = (ENV["SG_SERVER_PORT"]? || 3000).to_i
host = ENV["SG_SERVER_HOST"]? || "0.0.0.0"
process_count = (ENV["SG_PROCESS_COUNT"]? || System.cpu_count).to_i

# Command line options
OptionParser.parse(ARGV.dup) do |parser|
  parser.banner = "Usage: #{PROGRAM_NAME} [arguments]"

  parser.on("-b HOST", "--bind=HOST", "Specifies the server host") { |h| host = h }
  parser.on("-p PORT", "--port=PORT", "Specifies the server port") { |p| port = p.to_i }

  parser.on("-w COUNT", "--workers=COUNT", "Specifies the number of processes to handle requests") do |w|
    process_count = w.to_i
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

# Start clustering
#  process_count < 1 == `System.cpu_count` but this is not always accurate
server.cluster(process_count) if process_count != 1

terminate = Proc(Signal, Nil).new do |signal|
  puts " > terminating gracefully"
  spawn { server.close }
  signal.ignore
end

# Detect ctr-c to shutdown gracefully
Signal::INT.trap &terminate
# Docker containers use the term signal
Signal::TERM.trap &terminate

# Start the server
server.run do
  puts "Listening on #{server.print_addresses}"
end

# Shutdown message
puts "#{APP_NAME} leaps through the veldt\n"
