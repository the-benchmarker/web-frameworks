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
    @connections = 1000.0
    @init = false

    OptionParser.parse! do |parser|
      parser.banner = "Usage: time ./bin/benchmark [options]"
      parser.on("-t THREADS", "--threads THREADS", "# of threads") do |threads|
        @threads = threads.to_i
      end
      parser.on("-c CONNECTIONS", "--connections CONNECTIONS", "# of opened connections") do |connections|
        @connections = connections.to_f
      end
      parser.on("-d DURATION", "--duration DURATION", "Time to test, in seconds") do |duration|
        @duration = duration.to_i
      end
      parser.on("-u URL", "--url URL", "URL to call") do |url|
        @url = url
      end
      parser.on("-m METHOD", "--method METHOD", "HTTP method to use") do |method|
        @method = method
      end
      parser.on("-i", "--init", "Initialize (create json with 0 values)") do |x|
        @init = true
      end
    end
  end

  def run
    if @method == "POST"
      command = "wrk -H 'Connection: keep-alive' --latency -d #{@duration}s -s #{PIPELINE_POST} -c #{@connections.to_i} --timeout 8 -t #{@threads} #{@url}"
    else
      command = "wrk -H 'Connection: keep-alive' --latency -d #{@duration}s -s #{PIPELINE_GET} -c #{@connections.to_i} --timeout 8 -t #{@threads} #{@url}"
    end
    io = IO::Memory.new
    Process.run(command, shell: true, error: io)

    result = io.to_s.split(",")

    data = JSON.build do |json|
      json.object do
        json.field "request" do
          json.object do
            json.field "duration", (@init ? 0 : result[0].to_f)
            json.field "total", (@init ? 0 : result[1].to_f)
            json.field "per_second", (@init ? 0 : result[2].to_f)
            json.field "bytes", (@init ? 0 : result[3].to_f)
          end
        end

        json.field "error" do
          json.object do
            json.field "socket", (@init ? 0 : result[4].to_f)
            json.field "read", (@init ? 0 : result[5].to_f)
            json.field "write", (@init ? 0 : result[6].to_f)
            json.field "http", (@init ? 0 : result[7].to_f)
            json.field "timeout", (@init ? 0 : result[8].to_f)
          end
        end

        json.field "latency" do
          json.object do
            json.field "minimum", (@init ? 0 : result[9].to_f)
            json.field "maximum", (@init ? 0 : result[10].to_f)
            json.field "average", (@init ? 0 : result[11].to_f)
            json.field "deviation", (@init ? 0 : result[12].to_f)
          end
        end

        json.field "percentile" do
          json.object do
            json.field "fifty", (@init ? 0 : result[13].to_f)
            json.field "ninety", (@init ? 0 : result[14].to_f)
            json.field "ninety_nine", (@init ? 0 : result[15].to_f)
            json.field "ninety_nine_ninety", (@init ? 0 : result[16].to_f)
          end
        end
      end
    end

    STDOUT.puts data
  end
end

client = Client.new
client.run
