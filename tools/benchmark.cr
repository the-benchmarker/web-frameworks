require "json"
require "option_parser"
require "io/memory"
require "yaml"
require "kiwi/file_store"
require "admiral"

# Path for client binary
CLIENT = File.join(Dir.current, "bin", "client")

class Result
  JSON.mapping(
    request: {type: Request, nilable: false},
    error: {type: Error, nilable: false},
    latency: {type: Latency, nilable: false},
    percentile: {type: Percentile, nilable: false},
  )
end

class Request
  JSON.mapping(
    duration: Float64,
    total: Float64,
    bytes: Float64,
    per_second: Float64
  )
end

class Error
  JSON.mapping(
    socket: Float64,
    read: Float64,
    write: Float64,
    http: Float64,
    timeout: Float64
  )
end

class Latency
  JSON.mapping(
    maximum: Float64,
    minimum: Float64,
    average: Float64,
    deviation: Float64
  )
end

class Percentile
  JSON.mapping(
    fifty: Float64,
    ninety: Float64,
    ninety_nine: Float64,
    ninety_nine_ninety: Float64
  )
end

# Benchmark
# server : server context
# threads : number of thread to launch simultaneously
# connections : number of opened connections per thread
def benchmark(host, threads, connections, duration) : Hash(String, Hash(String, Float64))
  latency = 0.0
  requests = 0.0
  raw = `#{CLIENT} --url http://#{host} --init`
  result = Result.from_json(raw)
  parser = JSON::PullParser.new(raw)
  results = Hash(String, Hash(String, Float64)).new(parser)

  ["/", "/user/0"].each do |route|
    raw = `#{CLIENT} --duration #{duration} --connections #{connections.to_i.to_s} --threads #{threads} --url http://#{host}#{route}`
    result = Result.from_json(raw)
    parser = JSON::PullParser.new(raw)
    data = Hash(String, Hash(String, Float64)).new(parser)
    data.each do |key, metrics|
      results[key].merge!(metrics) { |_, v1, v2| v1 + (v2/3) }
    end
    requests = requests + result.request.per_second
    latency = latency + result.percentile.fifty
  end

  ["/user"].each do |route|
    raw = `#{CLIENT} --method POST --duration #{duration} --connections #{connections.to_i.to_s} --threads #{threads} --url http://#{host}#{route}`
    result = Result.from_json(raw)
    parser = JSON::PullParser.new(raw)
    data = Hash(String, Hash(String, Float64)).new(parser)
    data.each do |key, metrics|
      results[key].merge!(metrics) { |_, v1, v2| v1 + (v2/3) }
    end
    requests = requests + result.request.per_second
    latency = latency + result.percentile.fifty
  end

  return results
end

class App < Admiral::Command
  class Extract < Admiral::Command
    define_flag language : String, description: "language selected, to set-up environment", required: true, short: l
    define_flag framework : String, description: "framework that will be set-up", required: true, short: f

    def run
      config = Kiwi::FileStore.new("config.db")
      results = Kiwi::FileStore.new("results.db")
      ip = config.get("#{flags.framework.to_s.upcase}_IP").to_s
      raw = benchmark(ip, 8, 1000, 15) # 15s benchmark 8 thread - 1000 (keep-alive) connections per thread
      results.set("#{flags.language}:#{flags.framework}", raw.to_json)
    end
  end

  register_sub_command extract : Extract, description "Get result and store them"

  def run
    puts "help"
  end
end

App.run
