require "benchmark"

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
   ]},
  {lang: "crystal", targets: [
     {name: "kemal", repo: "kemalcr/kemal"},
     {name: "router_cr", repo: "tbrand/router.cr"},
   ]},
  {lang: "go", targets: [
     {name: "echo", repo: "labstack/echo"},
     {name: "gorilla_mux", repo: "gorilla/mux"},
     {name: "fasthttprouter", repo: "buaazp/fasthttprouter"},
   ]},
  {lang: "rust", targets: [
     {name: "iron", repo: "iron/iron"},
     {name: "nickel", repo: "nickel-org/nickel.rs"},
     {name: "rocket", repo: "SergioBenitez/Rocket"},
   ]},
  {lang: "node", targets: [
     {name: "express", repo: "expressjs/express"},
     {name: "clusterexpress", repo: "LearnBoost/cluster"},
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
   ]},
]

# struct for benchmark result
record BenchResult, max : Float64, min : Float64, ave : Float64, total : Float64
# struct for target
record Target, lang : String, name : String, repo : String

record Ranked, res : BenchResult, target : Target

# Executor of each server
class ExecServer
  def initialize(@target : Target)
    # Path of the executable
    executable = File.expand_path(PATH_PREFIX + "server_" + @target.lang + "_" + @target.name, __FILE__)
    # Running the server
    @process = Process.new(executable)
  end

  # Kill the server process
  def kill
    @process.not_nil!.kill

    # Since ruby's frameworks are running on puma, we have to kill the independent process
    if @target.lang == "ruby"
      kill_proc("puma")
    elsif @target.lang == "node"
      kill_proc("node")
    elsif @target.name == "plug"
      path = File.expand_path("../../../elixir/plug/_build/prod/rel/my_plug/bin/my_plug", __FILE__)
      Process.run("bash #{path} stop", shell: true)
    elsif @target.name == "phoenix"
      path = File.expand_path("../../../elixir/phoenix/_build/prod/rel/my_phoenix/bin/my_phoenix", __FILE__)
      Process.run("bash #{path} stop", shell: true)
    elsif @target.name == "akkahttp"
      kill_proc("sbt")
    elsif @target.name == "aspnetcore"
      kill_proc("dotnet")
    end
  end

  def kill_proc(proc : String)
    # Search pid of the process
    procs = `ps aux | grep #{proc} | grep -v grep`
    procs.split("\n").each do |proc|
      next if proc.includes?("benchmarker")
      proc.split(" ").each do |pid|
        if /\d+/ =~ pid
          _pid = $~[0].to_i
          Process.kill(Signal::TERM, _pid)
          break
        end
      end
    end
  end
end

# Running client and returning span
def client
  s = Time.now
  `#{CLIENT} -t 16 -r 1000`
  e = Time.now
  (e-s).to_f
end

# Benchmark
# server : server context
# count  : number of samples
def benchmark(server, count) : BenchResult
  max   : Float64 = -1.0
  min   : Float64 = 100000.0
  ave   : Float64 = 0.0
  total : Float64 = 0.0

  # Running server
  exec_server = ExecServer.new(server)

  # Wait for the binding
  sleep 7

  count.times do |i|
    span = client
    max = span if span > max
    min = span if span < min
    total += span
  end

  ave = total/count.to_f
  exec_server.kill

  result = BenchResult.new(max, min, ave, total)

  sleep 3

  result
end

def all_frameworks : Array(Target)
  targets = [] of Target

  LANGS.each do |lang|
    lang[:targets].each do |framework|
      targets.push(Target.new(lang[:lang], framework[:name], framework[:repo]))
    end
  end

  targets
end

def puts_markdown(line, f = nil)
  puts line
  f.puts line if f
end

targets = if ARGV.reject{ |opt| opt.starts_with?("--") }.size > 0
            all_frameworks.select{ |target| ARGV.includes?(target.lang) || ARGV.includes?(target.name) }
          else
            all_frameworks
          end

abort "No targets found for #{ARGV[0]}" if targets.size == 0

f = if ARGV.includes?("--record")
      File.open(File.expand_path("../../../res/README.md", __FILE__), "w")
    end

puts_markdown "## Result", f
puts_markdown "", f
puts_markdown "Last update: #{Time.now.to_s("%Y-%m-%d")}", f
puts_markdown ""
puts_markdown "Bechmark running..."

all   = [] of Ranked
ranks = [] of Ranked

targets.each do |target|
  result = benchmark(target, 5)
  puts_markdown "Done. <- #{target.name}"
  all.push(Ranked.new(result, target))
end

ranks = all.sort do |rank0, rank1|
  rank0.res.ave <=> rank1.res.ave
end

# --- Ranking of frameworks

puts_markdown "", f
puts_markdown "## Ranking (Framework)", f
puts_markdown "", f

rank = 1

ranks.each do |ranked|
  puts_markdown "#{rank}. [#{ranked.target.name}](https://github.com/#{ranked.target.repo})", f
  rank += 1
end

# --- Ranking of langages

puts_markdown "", f
puts_markdown "## Ranking (Language)", f
puts_markdown "", f

ranked_langs = [] of String
rank = 1

ranks.each do |ranked|
  next if ranked_langs.includes?(ranked.target.lang)
  puts_markdown "#{rank}. #{ranked.target.lang} ([#{ranked.target.name}](https://github.com/#{ranked.target.repo}))", f
  ranked_langs.push(ranked.target.lang)
  rank += 1
end

# --- Result of all frameworks

puts_markdown "", f
puts_markdown "### All frameworks", f
puts_markdown "", f
puts_markdown "| %-25s | %-25s | %15s | %15s | %15s |" % ["Language (Runtime)", "Framework (Middleware)", "Max [sec]", "Min [sec]", "Ave [sec]"], f
puts_markdown "|---------------------------|---------------------------|-----------------|-----------------|-----------------|", f

all.each do |framework|
  puts_markdown "| %-25s | %-25s | %15f | %15f | %15f |" % [framework.target.lang, framework.target.name, framework.res.max, framework.res.min, framework.res.ave], f
end

f.close if f
