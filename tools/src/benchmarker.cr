require "http/client"
require "benchmark"
require "option_parser"
require "json"
require "kiwi/memory_store"

####################
# # DEFAULT VALUES ##
####################

threads = (System.cpu_count + 1).to_i
requests = 100_000.0
record = false
check = false
store = Kiwi::MemoryStore.new

#################
# ### OPTIONS ###
#################

OptionParser.parse! do |parser|
  parser.banner = "Usage: time ./bin/benchmark [options]"
  parser.on("-t THREADS", "--threads=THREADS", "# of threads") do |x|
    threads = x.to_i
  end
  parser.on("-r REQUESTS", "--requests=REQUESTS", "# of iterations of requests") do |x|
    requests = x.to_i
  end
  parser.on("--record", "Record results in README.md") do
    record = true
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
LANGS = [
  {lang: "ruby", targets: [
    {name: "rails", repo: "rails/rails"},
    {name: "sinatra", repo: "sinatra/sinatra"},
    {name: "roda", repo: "jeremyevans/roda"},
    {name: "rack-routing", repo: "georgeu2000/rack-routing"},
    {name: "flame", repo: "AlexWayfer/flame"},
    {name: "hanami", repo: "hanami/hanami"},
  ]},
  {lang: "crystal", targets: [
    {name: "raze", repo: "samueleaton/raze"},
    {name: "kemal", repo: "kemalcr/kemal"},
    {name: "router.cr", repo: "tbrand/router.cr"},
    {name: "raze", repo: "samueleaton/raze" },
    {name: "amber", repo: "amberframework/amber"},
    {name: "lucky", repo: "luckyframework/lucky"},
    {name: "spider-gazelle", repo: "spider-gazelle/spider-gazelle"},
    {name: "prism", repo: "vladfaust/prism"},
  ]},
  {lang: "go", targets: [
    {name: "echo", repo: "labstack/echo"},
    {name: "gorilla-mux", repo: "gorilla/mux"},
    {name: "iris", repo: "kataras/iris"},
    {name: "fasthttprouter", repo: "buaazp/fasthttprouter"},
    {name: "gin", repo: "gin-gonic/gin"},
  ]},
  {lang: "rust", targets: [
    {name: "actix-web", repo: "actix/actix-web"},
    {name: "iron", repo: "iron/iron"},
    {name: "nickel", repo: "nickel-org/nickel.rs"},
    {name: "rocket", repo: "SergioBenitez/Rocket"},
  ]},
  {lang: "node", targets: [
    {name: "express", repo: "expressjs/express"},
    {name: "fastify", repo: "fastify/fastify"},
    {name: "polka", repo: "lukeed/polka"},
    {name: "rayo", repo: "GetRayo/rayo.js"},
    {name: "koa", repo: "koajs/koa"},
    {name: "restify", repo: "restify/node-restify"},
    {name: "hapi", repo: "hapijs/hapi"},
  ]},
  #{lang: "elixir", targets: [
  #  {name: "plug", repo: "elixir-lang/plug"},
  #  {name: "phoenix", repo: "phoenixframework/phoenix"},
  #]},
  {lang: "swift", targets: [
    {name: "vapor", repo: "vapor/vapor"},
    {name: "perfect", repo: "PerfectlySoft/Perfect"},
    {name: "kitura", repo: "IBM-Swift/Kitura"},
  ]},
  {lang: "scala", targets: [
    {name: "akkahttp", repo: "akka/akka-http"},
  ]},
  {lang: "csharp", targets: [
    {name: "aspnetcore", repo: "aspnet/Home"},
  ]},
  {lang: "python", targets: [
    {name: "japronto", repo: "squeaky-pl/japronto"},
    {name: "sanic", repo: "channelcat/sanic"},
    {name: "flask", repo: "pallets/flask"},
    {name: "django", repo: "django/django"},
    {name: "tornado", repo: "tornadoweb/tornado"},
    {name: "vibora", repo: "vibora-io/vibora"},
  ]},
  {lang: "nim", targets: [
    #{name: "jester", repo: "dom96/jester"},
    {name: "mofuw", repo: "2vg/mofuw"},
  ]},
  {lang: "java", targets: [
    {name: "act", repo: "actframework/actframework"},
  ]},
  {lang: "cpp", targets: [
    {name: "evhtp", repo: "criticalstack/libevhtp"},
  ]},
  {lang: "php", targets: [
    {name: "symfony", repo: "symfony/symfony"},
    {name: "laravel", repo: "laravel/framework"},
  ]},
]

record Target, lang : String, name : String, repo : String
record Filter, req : Float64, lat : Float64
record Ranked, res : Filter, target : Target

def frameworks : Array(Target)
  targets = [] of Target

  LANGS.each do |lang|
    lang[:targets].each do |framework|
      targets.push(Target.new(lang[:lang], framework[:name], framework[:repo]))
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
def benchmark(host, threads, connections, target, store) : Filter
  latency = 0.0
  requests = 0.0
  raw = `#{CLIENT} --threads #{threads} --url http://#{host}:3000 --init`
  result = Result.from_json(raw)
  parser = JSON::PullParser.new(raw)
  results = Hash(String, Hash(String, Float64)).new(parser)

  ["/", "/user/0"].each do |route|
    raw = `#{CLIENT} --threads #{threads} --url http://#{host}:3000#{route}`
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
    raw = `#{CLIENT} --threads #{threads} --method "POST" --url http://#{host}:3000#{route}`
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
puts_markdown "threads: #{threads}, requests: #{requests}"
puts_markdown "```", m_lines, true
puts_markdown "Benchmark running ..."

all = [] of Ranked
ranks = [] of Ranked

targets.each do |target|
  cid = `docker run -td #{target.name}`.strip

  sleep 20 # due to external program usage

  remote_ip = `docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' #{cid}`.strip

  result = benchmark(remote_ip, threads, requests, target, store)

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
puts_markdown "<details open><summary>Ranked by latency (ordered by 50th percentile - lowest is better)</summary> ", m_lines, true
puts_markdown "", m_lines, true

puts_markdown "| %-25s | %-25s | %15s | %15s | %15s | %15s | %15s | %15s |" % ["Language (Runtime)", "Framework (Middleware)", "Average", "50% percentile", "90% percentile", "99% percentile", "99.9% percentile", "Standard deviation"], m_lines, true
puts_markdown "|---------------------------|---------------------------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|", m_lines, true

ranks_by_latency.each do |framework|
  raw = store.get("#{framework.target.lang}:#{framework.target.name}").as(String)
  result = Result.from_json(raw)
  puts_markdown "| %-25s | %-25s | %.2f ms | %.2f ms | %.2f ms | %.2f ms | %.2f ms | %.2f | " % [framework.target.lang, framework.target.name, (result.latency.average/1000), (result.percentile.fifty/1000), (result.percentile.ninety/1000), (result.percentile.ninety_nine/1000), (result.percentile.ninety_nine_ninety/1000), (result.latency.deviation)], m_lines, true
end

puts_markdown "", m_lines, true
puts_markdown "</details>", m_lines, true
puts_markdown "", m_lines, true

puts_markdown "", m_lines, true
puts_markdown "<details><summary>Ranked by requests (ordered by number or requests per sencond - highest is better)</summary>", m_lines, true
puts_markdown "", m_lines, true

puts_markdown "| %-25s | %-25s | %15s | %15s |" % ["Language (Runtime)", "Framework (Middleware)", "Requests / s", "Throughput"], m_lines, true
puts_markdown "|---------------------------|---------------------------|----------------:|---------:|", m_lines, true

ranks_by_requests.each do |framework|
  raw = store.get("#{framework.target.lang}:#{framework.target.name}").as(String)
  result = Result.from_json(raw)
  puts_markdown "| %-25s | %-25s | %.2f | %.2f MB |" % [framework.target.lang, framework.target.name, result.request.per_second, (result.request.bytes/1000000)], m_lines, true
end

puts_markdown "", m_lines, true
puts_markdown "</details>", m_lines, true
puts_markdown "", m_lines, true

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
