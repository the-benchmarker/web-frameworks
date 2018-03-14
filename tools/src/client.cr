require "http/client"
require "option_parser"

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

  macro run_spawn
    spawn do
      c = HTTP::Client.new @host, @port
      @requests.times do |t|
        r = c.get  "/"
        abort "status code should be 200 when GET /" if r.status_code != 200
        r = c.get  "/user/#{t}"
        abort "status code should be 200 when GET /user/:id" if r.status_code != 200
        abort "body should be '#{t}'" if r.body.lines.first != t.to_s
        r = c.post "/user"
        abort "status code should be 200 when POST /user" if r.status_code != 200
      end
      c.close
      channel.send(nil)
    end
  end

  def run
    channel = Channel(Nil).new

    @threads.times do |t|
      run_spawn
    end

    @threads.times do |t|
      channel.receive
    end
  end
end

client = Client.new
client.run
