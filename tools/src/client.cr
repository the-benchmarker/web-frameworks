require "json"
require "http/client"
require "option_parser"

PIPELINE = File.expand_path("../../" + "pipeline.lua", __FILE__)

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

    results = `wrk --threads 1 --script #{PIPELINE} --connections 100 --duration 1s --timeout 5 --latency http://#{@host}:#{@port}/`.strip.lines    
    latency = results[3].split
    request = results[4].split
    summary = results[10].split
    errors = File.read("/tmp/which_is_the_fastest.out").strip
    File.delete("/tmp/which_is_the_fastest.out")
    
    data = JSON.build do |json|
      json.object do
	json.field "errors", errors.to_i
        
        json.field "latency" do
          json.object do
            json.field "average", latency[1]
            json.field "stdev", latency[2]
            json.field "max", latency[3]
            json.field "pmstdev", latency[4]
          end
        end

        json.field "request" do
          json.object do
            json.field "average", request[1]
            json.field "stdev", request[2]
            json.field "max", request[3]
            json.field "pmstdev", request[4]
	    json.field "total", summary.first.to_i
	    json.field "per_second", results[11].split(":").last.strip.to_f64
          end
        end
        
        json.field "percentile" do
          json.object do
            json.field "fifty", results[6].split.last
            json.field "seventy_five", results[7].split.last
            json.field "ninety", results[8].split.last
            json.field "ninety_nine", results[9].split.last
          end
        end
        
        json.field "throughput" do
          json.object do
            json.field "total", summary[4]
            json.field "duration", summary[3]
            json.field "per_second", results[12].split(":").last.strip
          end
        end
        
      end
    end
    
    STDOUT.puts data
  end
end

client = Client.new
client.run
