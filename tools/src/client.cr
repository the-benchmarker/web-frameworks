require "http/client"
require "option_parser"

CLIENT = File.expand_path("../../" + "pipeline.lua", __FILE__)

class Client
  def initialize
    @threads  = 16
    @requests = 1000
    @host = "localhost"
    @port = 3000

    OptionParser.parse! do |parser|
      parser.banner = "Usage: time ./bin/benchmark [options]"
      parser.on("-t THREADS", "--threads=THREADS", "# of threads") do |threads|
        @threads = threads.to_i
      end
      parser.on("-r REQUESTS", "--requests=REQUESTS", "# of iterations of requests") do |requests|
        @requests = requests.to_i
      end
      parser.on("-h HOST", "--host=HOST", "IP / address of host to test") do |host|
        @host = host
      end
      parser.on("-p PORT", "--port=PORT", "Port to test") do |port|
	@port = port.to_i
      end
    end
  end

  def run
    `wrk --script #{CLIENT} http://#{@host}:#{@port}/`
  end
end

client = Client.new
client.run
