require "json"
require "http/client"
require "option_parser"

PIPELINE_GET  = File.expand_path("../../" + "pipeline.lua", __FILE__)
PIPELINE_POST = File.expand_path("../../" + "pipeline_post.lua", __FILE__)

class Client
  def initialize
    @threads = 16
    @duration = 15
    @url = ""
    @method = "GET"
    @connections = 1000

    OptionParser.parse! do |parser|
      parser.banner = "Usage: time ./bin/benchmark [options]"
      parser.on("-t THREADS", "--threads=THREADS", "# of threads") do |threads|
        @threads = threads.to_i
      end
      parser.on("-c CONNECTIONS", "--requests=CONNECTIONS", "# of opened connections") do |connections|
        @connections = connections.to_i
      end
      parser.on("-u URL", "--url=URL", "URL to call") do |url|
        @url = url
      end
      parser.on("-m METHOD", "--method=METHOD", "HTTP method to use") do |method|
        @method = method
      end
    end
  end

  def run
    if @method == "POST"
      `wrk -H 'Connection: keep-alive' --latency -d #{@duration}s -s #{PIPELINE_POST} -c #{@connections} --timeout 8 -t #{@threads} #{@url}`
    else
      `wrk -H 'Connection: keep-alive' --latency -d #{@duration}s -s #{PIPELINE_GET} -c #{@connections} --timeout 8 -t #{@threads} #{@url}`
    end

    result = File.read("/tmp/which_is_the_fastest.out").split(",")
    File.delete("/tmp/which_is_the_fastest.out")

    data = JSON.build do |json|
      json.object do
        json.field "duration", result[0].to_f

        json.field "request" do
          json.object do
            json.field "total", result[1].to_f
            json.field "per_second", result[2].to_f
            json.field "bytes", result[3].to_f
          end
        end

        json.field "error" do
          json.object do
            json.field "socket", result[4].to_f
            json.field "read", result[5].to_f
            json.field "write", result[6].to_f
            json.field "http", result[7].to_f
            json.field "timeout", result[8].to_f
          end
        end

        json.field "latency" do
          json.object do
            json.field "minimum", result[9].to_f
            json.field "maximum", result[10].to_f
            json.field "average", result[11].to_f
            json.field "deviation", result[12].to_f
          end
        end

        json.field "percentile" do
          json.object do
            json.field "fifty", result[13].to_f
            json.field "ninety", result[14].to_f
            json.field "ninety_nine", result[15].to_f
            json.field "ninety_nine_ninety", result[16].to_f
          end
        end
      end
    end

    STDOUT.puts data
  end
end

client = Client.new
client.run
