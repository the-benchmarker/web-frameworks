require "admiral"
require "pg"

PIPELINES = {
  "GET": File.expand_path("../../" + "pipeline.lua", __FILE__),
}

def insert(framework_id, metric, value, concurrency_level_id)
  DB.open(ENV["DATABASE_URL"]) do |db|
    row = db.query("INSERT INTO keys (label) VALUES ($1) ON CONFLICT (label) DO UPDATE SET label = $1 RETURNING id", metric)
    row.move_next
    metric_id = row.read(Int)

    row = db.query("INSERT INTO values (key_id, value) VALUES ($1, $2) RETURNING id", metric_id, value)
    row.move_next
    value_id = row.read(Int)

    db.exec("INSERT INTO metrics (value_id, framework_id, concurrency_id) VALUES ($1, $2, $3)", value_id, framework_id, concurrency_level_id)
  end
end

class Client < Admiral::Command
  define_help
  define_flag threads : Int32, description: "# of threads", default: 8, long: "threads", short: "t"
  define_flag duration : Int32, description: "Time to test, in seconds", default: 15, long: "duration", short: "d"
  define_flag language : String, description: "Language used", required: true, long: "language", short: "l"
  define_flag framework : String, description: "Framework used", required: true, long: "framework", short: "f"
  define_flag concurrencies : Array(Int32), description: "Concurrency level", required: true, long: "concurrency", short: "c"
  define_flag routes : Array(String), long: "routes", short: "r", default: ["GET:/"]
  define_flag host : String, description: "Host", short: "h", required: true, long: "hostname"

  def run
    db = DB.open(ENV["DATABASE_URL"])

    row = db.query("INSERT INTO languages (label) VALUES ($1) ON CONFLICT (label) DO UPDATE SET label = $1 RETURNING id", flags.language)
    row.move_next
    language_id = row.read(Int)

    row = db.query("INSERT INTO frameworks (language_id, label) VALUES ($1, $2) ON CONFLICT (language_id, label) DO UPDATE SET label = $2 RETURNING id", language_id, flags.framework)
    row.move_next
    framework_id = row.read(Int)

    address = "#{flags.host}:3000"

    # Run a 5-second primer at 8 client-concurrency to verify that the server is in fact running. These results are not captured.

    process = Process.new("wrk", ["-H", "Connection: keep-alive", "-d", "5s", "-c", "8", "--timeout", "8", "-t", flags.threads.to_s, "http://#{address}"])
    process.wait
    STDERR.puts "Warmup"

    # Run a 15-second warmup at 256 client-concurrency to allow lazy-initialization to execute and just-in-time compilation to run. These results are not captured.

    process = Process.new("wrk", ["-H", "Connection: keep-alive", "-d", "#{flags.duration}s", "-c", "256", "--timeout", "8", "-t", flags.threads.to_s, "http://#{address}"])
    process.wait
    STDERR.puts "Lazy-initialization"

    flags.routes.each do |route|
      method, uri = route.split(":")
      url = "http://#{address}#{uri}"

      flags.concurrencies.each do |concurrency|
        STDERR.puts "Sieging at concurrency level => #{concurrency}"
        row = db.query("INSERT INTO concurrencies (level) VALUES ($1) ON CONFLICT (level) DO UPDATE SET level = $1 RETURNING id", concurrency)
        row.move_next
        concurrency_level_id = row.read(Int)

        options = {} of String => String | Int32
        options["duration"] = "#{flags.duration}s"
        options["connections"] = concurrency
        options["timeout"] = 8
        options["threads"] = flags.threads
        if method == "POST"
          options["script"] = File.expand_path("../../" + "pipeline_post.lua", __FILE__)
        end
        params = [] of String
        params << "--latency"
        options.each do |key, value|
          params << "--#{key}"
          params << value.to_s
        end
        params << url

        stdout = IO::Memory.new
        process = Process.new("wrk", params, output: stdout)
        status = process.wait
        output = stdout.to_s
        lines = output.split("\n")
        requests_per_seconds = lines.grep(/Requests/).first.split(":").pop.strip.to_f

        # insert(framework_id, "request_duration", result[0])
        # insert(framework_id, "request_total", result[1])
        insert(framework_id, "request_per_second", requests_per_seconds, concurrency_level_id)
        # insert(framework_id, "request_bytes", result[3])

        # insert(framework_id, "error_socket", result[4])
        # insert(framework_id, "error_read", result[5])
        # insert(framework_id, "error_write", result[6])
        # insert(framework_id, "error_http", result[7])
        # insert(framework_id, "error_timeout", result[8])

        # insert(framework_id, "latency_minimum", result[9])
        # insert(framework_id, "latency_maximum", result[10])
        # insert(framework_id, "latency_average", result[11])
        # insert(framework_id, "latency_deviation", result[12])

        # insert(framework_id, "percentile_fifty", result[13])
        # insert(framework_id, "percentile_ninety", result[14])
        # insert(framework_id, "percentile_ninety_nine", result[15])
        # insert(framework_id, "percentile_ninety_nine_ninety", result[16])
        puts "End @ #{concurrency}"
      end
    end

    db.close
  end
end

Client.run
