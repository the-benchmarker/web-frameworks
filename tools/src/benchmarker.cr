require "http/client"
require "benchmark"
require "option_parser"
require "json"

####################
# # DEFAULT VALUES ##
####################

threads = (System.cpu_count + 1).to_i
requests = 100_000.0
record = false
check = false

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
  parser.on("--check", "Check mode (without result, typically run on CI)") do
    check = true
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
  ]},
  {lang: "crystal", targets: [
    {name: "kemal", repo: "kemalcr/kemal"},
    {name: "router.cr", repo: "tbrand/router.cr"},
    {name: "amber", repo: "amberframework/amber"},
    {name: "lucky", repo: "luckyframework/lucky"},
    {name: "spider-gazelle", repo: "spider-gazelle/spider-gazelle"},
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
  ]},
  {lang: "elixir", targets: [
    {name: "plug", repo: "elixir-lang/plug"},
    {name: "phoenix", repo: "phoenixframework/phoenix"},
  ]},
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
    {name: "sanic", repo: "channelcat/sanic"},
    {name: "japronto", repo: "squeaky-pl/japronto"},
    {name: "flask", repo: "pallets/flask"},
    {name: "django", repo: "django/django"},
    {name: "tornado", repo: "tornadoweb/tornado"},
  ]},
  {lang: "nim", targets: [
    {name: "jester", repo: "dom96/jester"},
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
  ]}
]

record Target, lang : String, name : String, repo : String

record BenchResult, request : Float64, latency : Float64, percentile : Float64, throughput : Float64

record Ranked, res : BenchResult, target : Target

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
    duration: Float64,
    request: {type: Request, nilable: false},
    error: {type: Error, nilable: false},
    latency: {type: Latency, nilable: false},
    percentile: {type: Percentile, nilable: false},
  )
end

class Request
  JSON.mapping(
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
def benchmark(host, threads, connections) : BenchResult
  request = 0.0
  latency = 0.0
  percentile = 0.0
  throughput = 0.0
  ["/", "/user/0"].each do |route|
    raw = `#{CLIENT} --threads #{threads} --url http://#{host}:3000#{route}`
    result = Result.from_json(raw)
    request = request + result.request.per_second
    latency = latency + result.latency.average
    percentile = percentile + result.percentile.ninety_nine
    throughput = request + result.request.bytes
  end
  ["/user"].each do |route|
    raw = `#{CLIENT} --threads #{threads} --method "POST" --url http://#{host}:3000#{route}`
    result = Result.from_json(raw)
    request = request + result.request.per_second
    latency = latency + result.latency.average
    percentile = percentile + result.percentile.ninety_nine
    throughput = request + result.request.bytes
  end

  BenchResult.new((request/3), (latency/3), (percentile/3), (throughput/3))
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

  if check
    r = HTTP::Client.get "http://#{remote_ip}:3000/"
    unless r.status_code == 200 && r.body.empty?
      STDERR.puts "Fail to GET on / for #{target} : [#{r.status_code}] #{r.body}"
    end
    r = HTTP::Client.get "http://#{remote_ip}:3000/user/0"
    unless r.status_code == 200 && r.body.lines.first == "0"
      STDERR.puts "Fail to GET on /user/0 for #{target} : [#{r.status_code}] #{r.body}"
    end
    r = HTTP::Client.post "http://#{remote_ip}:3000/user"
    unless r.status_code == 200 && r.body.empty?
      STDERR.puts "Fail to POST on /user for #{target} : [#{r.status_code}] #{r.body}"
    end
  else
    result = benchmark(remote_ip, threads, requests)

    all.push(Ranked.new(result, target))
  end

  puts_markdown "Done. <- #{target.name}"

  `docker stop #{cid}`
end

unless check
  ranks = all.sort do |rank0, rank1|
    rank1.res.request <=> rank0.res.request
  end

  # --- Ranking of frameworks

  puts_markdown "", m_lines, true
  puts_markdown "### Ranking (Framework)", m_lines, true
  puts_markdown "", m_lines, true

  rank = 1

  ranks.each do |ranked|
    puts_markdown "#{rank}. [#{ranked.target.name}](https://github.com/#{ranked.target.repo}) (#{ranked.target.lang})", m_lines, true
    rank += 1
  end

  # --- Ranking of langages

  puts_markdown "", m_lines, true
  puts_markdown "### Ranking (Language)", m_lines, true
  puts_markdown "", m_lines, true

  ranked_langs = [] of String
  rank = 1

  ranks.each do |ranked|
    next if ranked_langs.includes?(ranked.target.lang)
    puts_markdown "#{rank}. #{ranked.target.lang} ([#{ranked.target.name}](https://github.com/#{ranked.target.repo}))", m_lines, true
    ranked_langs.push(ranked.target.lang)
    rank += 1
  end

  # --- Result of all frameworks

  puts_markdown "", m_lines, true
  puts_markdown "### All frameworks", m_lines, true
  puts_markdown "", m_lines, true

  puts_markdown "| %-25s | %-25s | %15s | %15s | %15s | %15s |" % ["Language (Runtime)", "Framework (Middleware)", "Requests / s", "Latency", "99 percentile", "Throughput"], m_lines, true
  puts_markdown "|---------------------------|---------------------------|----------------:|----------------:|----------------:|-----------:|", m_lines, true

  all.each do |framework|
    puts_markdown "| %-25s | %-25s | %.2f | %.2f | %.2f | %.2f MB |" % [framework.target.lang, framework.target.name, framework.res.request, framework.res.latency, framework.res.percentile, (framework.res.throughput/1000000)], m_lines, true
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
end
