require "admiral"
require "pg"

PIPELINES = {
  "GET":  File.expand_path("../../" + "pipeline.lua", __FILE__),
  "POST": File.expand_path("../../" + "pipeline_post.lua", __FILE__),
}

def insert(db, framework_id, metric, value)
  DB.open("postgresql://postgres@localhost/benchmark") do |db|
    row = db.exec "insert into keys (label) values ($1) on conflict do nothing", [metric]
    metric_id = row.last_insert_id
    # FIXME add method table to link metric
    if metric_id == 0
      metric_id = db.query_one("select id from keys where label = '#{metric}' limit 1", &.read(Int))
    end
    # FIXME returning not working
    row = db.exec "insert into values (key_id, value) values ($1,$2) returning id", [metric_id, value]
    value_id = row.last_insert_id
    if value_id == 0
      value_id = db.query_one("select id from values where key_id = #{metric_id} and value = #{value} order by id desc limit 1", &.read(Int))
    end
    db.exec "insert into metrics (value_id, framework_id) values ($1,$2)", [value_id, framework_id]
  end
end

class Client < Admiral::Command
  define_flag threads : Int32, description: "# of threads", default: 16, long: "threads", short: "t"
  define_flag connections : Int32, description: "# of opened connections", default: 1000, long: "connections", short: "c"
  define_flag duration : Int32, description: "Time to test, in seconds", default: 15, long: "duration", short: "d"
  define_flag language : String, description: "Language used", required: true, long: "language", short: "l"
  define_flag framework : String, description: "Framework used", required: true, long: "framework", short: "f"
  define_flag routes : Array(String), long: "routes", short: "r", default: ["GET:/"]

  def run
    db = DB.open("postgresql://postgres@localhost/benchmark")
    row = db.exec "insert into languages (label) values ($1) on conflict do nothing", [flags.language]
    language_id = row.last_insert_id
    if language_id == 0
      language_id = db.query_one("select id from languages where label = '#{flags.language}'", &.read(Int))
    end

    row = db.exec "insert into frameworks (language_id, label) values ($1, $2) on conflict do nothing", [language_id, flags.framework]
    framework_id = row.last_insert_id
    if framework_id == 0
      framework_id = db.query_one("select id from frameworks where language_id = #{language_id} and label = '#{flags.framework}'", &.read(Int))
    end

    sleep 20 # due to external program usage
    address = File.read("ip.txt").strip

    # Warm-up
    Process.new("wrk", ["-H", "Connection: keep-alive", "-d", "5s", "-c", "8", "--timeout", "8", "-t", flags.threads.to_s, "http://#{address}:3000"])

    flags.routes.each do |route|
      method, uri = route.split(":")
      url = "http://#{address}:3000#{uri}"

      pipeline = PIPELINES[method]

      command = "wrk -H 'Connection: keep-alive' --latency -d #{flags.duration}s -s #{pipeline} -c #{flags.connections} --timeout 8 -t #{flags.threads} #{url}"

      io = IO::Memory.new
      Process.run(command, shell: true, error: io)

      result = io.to_s.split(",")

      insert(db, framework_id, "request_duration", result[0])
      insert(db, framework_id, "request_total", result[1])
      insert(db, framework_id, "request_per_second", result[2])
      insert(db, framework_id, "request_bytes", result[3])

      insert(db, framework_id, "error_socket", result[4])
      insert(db, framework_id, "error_read", result[5])
      insert(db, framework_id, "error_write", result[6])
      insert(db, framework_id, "error_http", result[7])
      insert(db, framework_id, "error_timeout", result[8])

      insert(db, framework_id, "latency_minimum", result[9])
      insert(db, framework_id, "latency_maximum", result[10])
      insert(db, framework_id, "latency_average", result[11])
      insert(db, framework_id, "latency_deviation", result[12])

      insert(db, framework_id, "percentile_fifty", result[13])
      insert(db, framework_id, "percentile_ninety", result[14])
      insert(db, framework_id, "percentile_ninety_nine", result[15])
      insert(db, framework_id, "percentile_ninety_nine_ninety", result[16])

      db.close
    end
  end
end

Client.run
