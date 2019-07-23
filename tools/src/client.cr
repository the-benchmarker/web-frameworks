require "admiral"
require "redis"

PIPELINES = {
  "GET":  File.expand_path("../../" + "pipeline.lua", __FILE__),
  "POST": File.expand_path("../../" + "pipeline_post.lua", __FILE__),
}

class Client < Admiral::Command
  define_flag threads : Int32, description: "# of threads", default: 16, long: "threads", short: "t"
  define_flag connections : Int32, description: "# of opened connections", default: 1000, long: "connections", short: "c"
  define_flag duration : Int32, description: "Time to test, in seconds", default: 15, long: "duration", short: "d"
  define_flag framework : String, description: "Framework used", required: true, long: "host", short: "f"
  define_flag routes : Array(String), long: "routes", short: "r", default: ["GET:/"]

  def run
    cid = `docker run -td #{flags.framework}`.strip
    sleep 20 # due to external program usage
    ip = `docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' #{cid}`.strip
    flags.routes.each do |route|
      method, uri = route.split(":")
      url = "http://#{ip}:3000#{uri}"

      pipeline = PIPELINES[method]

      command = "wrk -H 'Connection: keep-alive' --latency -d #{flags.duration}s -s #{pipeline} -c #{flags.connections} --timeout 8 -t #{flags.threads} #{url}"

      io = IO::Memory.new
      Process.run(command, shell: true, error: io)

      result = io.to_s.split(",")

      redis = Redis.new

      redis.set("request:duration", result[0].to_f)
      redis.set("request:total", result[1].to_f)
      redis.set("request:per_second", result[2].to_f)
      redis.set("request:bytes", result[3].to_f)

      redis.set("error:socket", result[4].to_f)
      redis.set("error:read", result[5].to_f)
      redis.set("error:write", result[6].to_f)
      redis.set("error:http", result[7].to_f)
      redis.set("error:timeout", result[8].to_f)

      redis.set("latency:minimum", result[9].to_f)
      redis.set("latency:maximum", result[10].to_f)
      redis.set("latency:average", result[11].to_f)
      redis.set("latency:deviation", result[12].to_f)

      redis.set("percentile:fifty", result[13].to_f)
      redis.set("percentile:ninety", result[14].to_f)
      redis.set("percentile:ninety_nine", result[15].to_f)
      redis.set("percentile:ninety_nine_ninety", result[16].to_f)
    end
  end
end

Client.run
