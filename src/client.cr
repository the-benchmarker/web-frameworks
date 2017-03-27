require "http/client"
require "option_parser"

class Client

  def initialize
    @threads = 16
    @requests = 1000

    OptionParser.parse! do |parser|
      
      parser.banner = "Usage: time ./bin/benchmark [options]"
      
      parser.on("-t THREADS", "--threads=THREADS", "# of threads") do |threads|
        @threads = threads.to_i
      end

      parser.on("-r REQUESTS", "--requests=REQUESTS", "# of iterations of requests") do |requests|
        @requests = requests.to_i
      end
    end
  end

  macro run_spawn
    
    spawn do

      c = HTTP::Client.new "localhost", 3000

      @requests.times do |t|
        c.get  "/"
        c.get  "/user/3"
        c.post "/user"
      end

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

    puts "#{@threads * (@requests * 3)} requests in total"
  end
end

client = Client.new
client.run
