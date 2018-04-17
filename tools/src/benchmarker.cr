require "benchmark"
require "option_parser"
require "json"

####################
## DEFAULT VALUES ##
####################

threads = (System.cpu_count + 1).to_i
requests = 100_000.0
record = false

#################
#### OPTIONS ####
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

################
## FRAMEWORKS ##
################

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
    {name: "rack-routing", repo: "iAmPlus/rack-routing"},
  ]},
  {lang: "crystal", targets: [
    {name: "kemal", repo: "kemalcr/kemal"},
    {name: "router.cr", repo: "tbrand/router.cr"},
    {name: "raze", repo: "samueleaton/raze"},
    {name: "lucky", repo: "luckyframework/lucky"},
    {name: "amber", repo: "amberframework/amber"},
    {name: "spider-gazelle", repo: "spider-gazelle/spider-gazelle"},
  ]},
  {lang: "go", targets: [
    {name: "echo", repo: "labstack/echo"},
    {name: "gorilla_mux", repo: "gorilla/mux"},
    {name: "iris", repo: "kataras/iris"},
    {name: "fasthttprouter", repo: "buaazp/fasthttprouter"},
    {name: "gin", repo: "gin-gonic/gin"},
  ]},
  {lang: "rust", targets: [
    {name: "actix", repo: "actix/actix-web"},
    {name: "iron", repo: "iron/iron"},
    {name: "nickel", repo: "nickel-org/nickel.rs"},
    {name: "rocket", repo: "SergioBenitez/Rocket"},
  ]},
  {lang: "node", targets: [
    {name: "express", repo: "expressjs/express"},
    {name: "polka", repo: "lukeed/polka"},
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
]

# struct for benchmark result
record BenchResult, max : Float64, min : Float64, ave : Float64, total : Float64

# struct for target
record Target, lang : String, name : String, repo : String

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

# Benchmark
# server : server context
# count  : number of samples
# threads : number of thread to launch simultaneously
# requests : numbers of request per thread
def benchmark(host, count, threads, requests) : BenchResult
  max : Float64 = -1.0
  min : Float64 = 100_000.0
  ave : Float64 = 0.0
  total : Float64 = 0.0

  count.times do |i|
    span = client(host, threads, requests)
    max = span if span > max
    min = span if span < min
    total += span
  end

  ave = total/count.to_f

  result = BenchResult.new(max, min, ave, total)

  sleep 5

  result
end

# Running client and returning span
# host: Hostname, or IP address, to target
# threads : number of thread to launch simultaneously
# requests : numbers of request per thread
def client(host, threads, requests)
  s = Time.now
  `#{CLIENT} -h #{host} -t #{threads.to_i} -r #{requests.to_i}`
  e = Time.now
  (e - s).to_f
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
  cid = `docker run -p 3000:3000 -td #{target.name}`.strip

  sleep 10 # due to external program usage

  result = benchmark("localhost", 5, threads, requests)

  puts_markdown "Done. <- #{target.name}"

  all.push(Ranked.new(result, target))

  `docker stop #{cid}`
end

ranks = all.sort do |rank0, rank1|
  rank0.res.ave <=> rank1.res.ave
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
puts_markdown "| %-25s | %-25s | %15s | %15s | %15s |" % ["Language (Runtime)", "Framework (Middleware)", "Max [sec]", "Min [sec]", "Ave [sec]"], m_lines, true
puts_markdown "|---------------------------|---------------------------|-----------------|-----------------|-----------------|", m_lines, true

all.each do |framework|
  puts_markdown "| %-25s | %-25s | %15f | %15f | %15f |" % [framework.target.lang, framework.target.name, framework.res.max, framework.res.min, framework.res.ave], m_lines, true
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
