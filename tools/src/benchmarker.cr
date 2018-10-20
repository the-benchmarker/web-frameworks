require "http/client"
require "benchmark"
require "option_parser"
require "json"
require "kiwi/memory_store"
require "yaml"

####################
# # DEFAULT VALUES ##
####################

threads = (System.cpu_count + 1).to_i
connections = 1000
record = false

check = false
store = Kiwi::MemoryStore.new
duration = 15

class Options
  class_property with_experimental : Bool = false
end

#################
# ### OPTIONS ###
#################

OptionParser.parse! do |parser|
  parser.banner = "Usage: time ./bin/benchmark [options]"
  parser.on("-t THREADS", "--threads THREADS", "# of threads") do |x|
    threads = x
  end
  parser.on("-c CONNECTIONS", "--connections CONNECTIONS", "# of iterations of requests") do |x|
    connections = x
  end
  parser.on("-d DURATION", "--duration DURATION", "Time to test, in seconds") do |x|
    duration = x
  end
  parser.on("--record", "Record results in README.md") do
    record = true
  end
  parser.on("--experimental", "Display experimental stuff (non-native, not versioned)") do
    Options.with_experimental = true
  end
end

##################
# ## FRAMEWORKS ##
##################

# Prefix of pathes for each executable
PATH_PREFIX = "../../../bin/"

# Path for client binary
CLIENT = File.expand_path(PATH_PREFIX + "client", __FILE__)

# Each framework
record Target, lang : String, name : String, link : String, version : String, langver : String
record Filter, req : Float64, lat : Float64
record Ranked, res : Filter, target : Target

def frameworks : Array(Target)
  targets = [] of Target

  YAML.parse(File.read("FRAMEWORKS.yml")).as_h.each do |lang, data|
    data.as_h.each do |framework, line|
      row = line.as_h
      next if row.has_key?("type") && row["type"].to_s == "experimental" && !Options.with_experimental
      if row.has_key?("github")
        link = "github.com/#{row["github"]}"
      elsif row.has_key?("website")
        link = row["website"]
      end
      target = Target.new(lang.as_s, framework.as_s, "http://#{link.to_s}", row["version"].to_s, row["language"].to_s)
      targets.push(target)
    end
  end

  targets
end

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
# target : target
# store : in-memory storage used for results
def benchmark(host, threads, connections, duration, target, store) : Filter
  latency = 0.0
  requests = 0.0
  raw = `#{CLIENT} --url http://#{host}:3000 --init`
  result = Result.from_json(raw)
  parser = JSON::PullParser.new(raw)
  results = Hash(String, Hash(String, Float64)).new(parser)

  ["/", "/user/0"].each do |route|
    raw = `#{CLIENT} --duration #{duration} --connections #{connections.to_i.to_s} --threads #{threads} --url http://#{host}:3000#{route}`
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
    raw = `#{CLIENT} --method POST --duration #{duration} --connections #{connections.to_i.to_s} --threads #{threads} --url http://#{host}:3000#{route}`
    result = Result.from_json(raw)
    parser = JSON::PullParser.new(raw)
    data = Hash(String, Hash(String, Float64)).new(parser)
    data.each do |key, metrics|
      results[key].merge!(metrics) { |_, v1, v2| v1 + (v2/3) }
    end
    requests = requests + result.request.per_second
    latency = latency + result.percentile.fifty
  end

  store.set("#{target.lang}:#{target.name}", results.to_json)

  Filter.new((requests/3), (latency/3))
end

m_lines = [] of String

def puts_markdown(line, m_lines = nil, m = false)
  puts line
  m_lines.push(line) if m && m_lines
end

targets = [] of Target
frameworks.each do |target|
  if ARGV.includes?(target.lang) || ARGV.includes?(target.name)
    targets << target
  end
end

if targets.size == 0
  targets = frameworks
end

puts_markdown "Last update: #{Time.now.to_s("%Y-%m-%d")}", m_lines, true
puts_markdown "```", m_lines, true
puts_markdown "OS: #{`uname -s`.rstrip} (version: #{`uname -r`.rstrip}, arch: #{`uname -m`.rstrip})", m_lines, true
puts_markdown "CPU Cores: #{System.cpu_count}", m_lines, true
puts_markdown "threads: #{threads}, connections: #{connections}"
puts_markdown "```", m_lines, true
puts_markdown "Benchmark running ..."

all = [] of Ranked
ranks = [] of Ranked
emojis = [] of String
emojis << "one"
emojis << "two"
emojis << "three"
emojis << "four"
emojis << "five"

targets.each do |target|
  cid = `docker run -td #{target.name}`.strip

  sleep 20 # due to external program usage

  remote_ip = `docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' #{cid}`.strip

  result = benchmark(remote_ip, threads, connections, duration, target, store)

  all.push(Ranked.new(result, target))

  puts_markdown "Done. <- #{target.name}"

  `docker stop #{cid}`
end

ranks_by_requests = all.sort do |rank0, rank1|
  rank1.res.req <=> rank0.res.req
end

ranks_by_latency = all.sort do |rank0, rank1|
  rank0.res.lat <=> rank1.res.lat
end

# --- Ranking of frameworks

puts_markdown "", m_lines, true
puts_markdown "### Latency", m_lines, true
puts_markdown "", m_lines, true

puts_markdown "", m_lines, true
puts_markdown "#### Ranking (top 5)", m_lines, true
puts_markdown "", m_lines, true

ranks = ranks_by_latency[0...5]
ranks.each_with_index do |framework, i|
  puts_markdown "", m_lines, true
  puts_markdown ":%s: %s (%s)" % [emojis[i], framework.target.name, framework.target.lang], m_lines, true
  puts_markdown "", m_lines, true
end

puts_markdown "", m_lines, true
puts_markdown "#### Full table", m_lines, true
puts_markdown "", m_lines, true

puts_markdown "| %s | %s | %s | %s | %s | %s | %s | %s |" % ["Language (Runtime)", "Framework (Middleware)", "Average", "50th percentile", "90th percentile", "99th percentile", "99.9th percentile", "Standard deviation"], m_lines, true
puts_markdown "|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|", m_lines, true

ranks_by_latency.each do |framework|
  raw = store.get("#{framework.target.lang}:#{framework.target.name}").as(String)
  result = Result.from_json(raw)
  puts_markdown "| %s (%s) | [%s](%s) (%s) | %.2f ms | %.2f ms | %.2f ms | %.2f ms | %.2f ms | %.2f | " % [framework.target.lang, framework.target.langver, framework.target.name, framework.target.link, framework.target.version, (result.latency.average/1000), (result.percentile.fifty/1000), (result.percentile.ninety/1000), (result.percentile.ninety_nine/1000), (result.percentile.ninety_nine_ninety/1000), (result.latency.deviation)], m_lines, true
end

puts_markdown "", m_lines, true
puts_markdown "### Requests per seconds", m_lines, true
puts_markdown "", m_lines, true

puts_markdown "", m_lines, true
puts_markdown "#### Ranking (top 5)", m_lines, true
puts_markdown "", m_lines, true

ranks = ranks_by_requests[0...5]
ranks.each_with_index do |framework, i|
  puts_markdown "", m_lines, true
  puts_markdown ":%s: (%s) (%s)" % [emojis[i], framework.target.name, framework.target.lang], m_lines, true
  puts_markdown "", m_lines, true
end

puts_markdown "", m_lines, true
puts_markdown "#### Full table", m_lines, true
puts_markdown "", m_lines, true

puts_markdown "| %s | %s | %s | %s |" % ["Language (Runtime)", "Framework (Middleware)", "Requests / s", "Throughput"], m_lines, true
puts_markdown "|---------------------------|---------------------------|----------------:|---------:|", m_lines, true

ranks_by_requests.each do |framework|
  raw = store.get("#{framework.target.lang}:#{framework.target.name}").as(String)
  result = Result.from_json(raw)
  puts_markdown "| %s (%s) | [%s](%s) (%s) | %.2f | %.2f MB |" % [framework.target.lang, framework.target.langver, framework.target.name, framework.target.link, framework.target.version, result.request.per_second, (result.request.bytes/1000000)], m_lines, true
end

if record
  path = File.expand_path("../../../README.md", __FILE__)
  tag_from = "<!-- Result from here -->"
  tag_till = "<!-- Result till here -->"
  m_lines.insert(0, tag_from)
  m_lines.push(tag_till)
  prev_readme = File.read(path)
  next_readme = prev_readme.gsub(
    /\<!--\sResult\sfrom\shere\s-->[\s\S]*?<!--\sResult\still\shere\s-->/,
    m_lines.join('\n'))

  File.write(path, next_readme)
end
