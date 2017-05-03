require "benchmark"

# Prefix of pathes for each executable
PATH_PREFIX = "../../../bin/"

# Path for client binary
CLIENT = File.expand_path(PATH_PREFIX + "client", __FILE__)

# Each framework
LANGS = [
  {lang: "Ruby", targets: [
     {name: "Rails", exec: "server_ruby_rails"},
     {name: "Sinatra", exec: "server_ruby_sinatra"},
     {name: "Roda", exec: "server_ruby_roda"},
   ]},
  {lang: "Crystal", targets: [
     {name: "Kemal", exec: "server_crystal_kemal"},
     {name: "route.cr", exec: "server_crystal_route_cr"},
   ]},
  {lang: "Go", targets: [
     {name: "Echo", exec: "server_go_echo"},
     {name: "gorilla/mux", exec: "server_go_gorilla_mux"},
   ]},
  {lang: "Rust", targets: [
     {name: "IRON", exec: "server_rust_iron"},
     {name: "nickel.rs", exec: "server_rust_nickel"},
   ]},
  {lang: "node", targets: [
     {name: "express", exec: "server_node_express"},
   ]},
]

# struct for benchmark result
record BenchResult, max : Float64, min : Float64, ave : Float64, total : Float64 

# Executor of each server
class ExecServer
  def initialize(@server : NamedTuple(name: String, exec: String))
    # Path of the executable
    executable = File.expand_path(PATH_PREFIX + @server[:exec], __FILE__)
    # Running the server
    @process = Process.new(executable)
  end

  # Kill the server process
  def kill
    @process.not_nil!.kill

    # Since ruby's frameworks are running on puma, we have to kill the independent process
    if @server[:name] == "Rails" ||
       @server[:name] == "Roda" ||
       @server[:name] == "Sinatra"
      kill_proc("puma")
    elsif @server[:name] == "express"
      kill_proc("node")
    end
  end

  def kill_proc(proc : String)
    # Search pid of the process
    proc = `ps aux | grep #{proc} | grep -v grep`
    proc.split(" ").each do |pid|
      if /\d+/ =~ pid
        _pid = $~[0].to_i
        Process.kill(Signal::TERM, _pid)
        break
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
  sleep 5

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

def header(lang : String, name : String, max : String, min : String, ave : String)
  puts "%-15s %-25s %15s %15s %15s" % [lang, name, max, min, ave]
end

def result_line(lang : String, name : String, max : Float64, min : Float64, ave : Float64)
  puts "%-15s %-25s %15f %15f %15f" % [lang, name, max, min, ave]
end

header("Language", "Framework (Middleware)", "Max [sec]", "Min [sec]", "Ave [sec]")
header("-" * 15, "-" * 25, "-" * 15, "-" * 15, "-" * 15)

# Running benchmark for each server
LANGS.each do |lang|
  lang[:targets].each do |framework|
    result = benchmark(framework, 5)
    result_line(lang[:lang], framework[:name], result.max, result.min, result.ave)
  end
end
