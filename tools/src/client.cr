require "admiral"
require "sqlite3"

PIPELINES = {
  "GET":  File.expand_path("../../" + "pipeline.lua", __FILE__),
  "POST": File.expand_path("../../" + "pipeline_post.lua", __FILE__),
}

def insert(db, framework_id, metric, value)
  row = db.exec "insert or ignore into metric_keys values (null, ?, ?)", metric, framework_id
  metric_id = row.last_insert_id
  if metric_id == 0
    metric_id = db.scalar "select id from metric_keys where label = ? and framework_id = ?", metric, framework_id
  end
  db.exec "insert into metric_values values (null, ?, ?)", metric_id, value
end

class Client < Admiral::Command
  define_flag threads : Int32, description: "# of threads", default: 16, long: "threads", short: "t"
  define_flag connections : Int32, description: "# of opened connections", default: 1000, long: "connections", short: "c"
  define_flag duration : Int32, description: "Time to test, in seconds", default: 15, long: "duration", short: "d"
  define_flag language : String, description: "Language used", required: true, long: "language", short: "l"
  define_flag framework : String, description: "Framework used", required: true, long: "framework", short: "f"
  define_flag routes : Array(String), long: "routes", short: "r", default: ["GET:/"]

  def run
    db = DB.open "sqlite3://../../data.db"
    row = db.exec "insert or ignore into languages values (null, ?)", flags.language
    language_id = row.last_insert_id
    if language_id == 0
      language_id = db.scalar "select id from languages where label = ?", flags.language
    end

    row = db.exec "insert or ignore into frameworks values (null, ?, ?)", language_id, flags.framework
    framework_id = row.last_insert_id
    if language_id == 0
      framework_id = db.scalar "select id from languages where language_id = ? and label = ?", language_id, flags.framework
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

      insert(db, framework_id, "request:duration", result[0])
      insert(db, framework_id, "request:total", result[1])
      insert(db, framework_id, "request:per_second", result[2])
      insert(db, framework_id, "request:bytes", result[3])

      insert(db, framework_id, "error:socket", result[4])
      insert(db, framework_id, "error:read", result[5])
      insert(db, framework_id, "error:write", result[6])
      insert(db, framework_id, "error:http", result[7])
      insert(db, framework_id, "error:timeout", result[8])

      insert(db, framework_id, "latency:minimum", result[9])
      insert(db, framework_id, "latency:maximum", result[10])
      insert(db, framework_id, "latency:average", result[11])
      insert(db, framework_id, "latency:deviation", result[12])

      insert(db, framework_id, "percentile:fifty", result[13])
      insert(db, framework_id, "percentile:seventy_five", result[14])
      insert(db, framework_id, "percentile:ninety", result[15])
      insert(db, framework_id, "percentile:ninety_nine", result[16])
      insert(db, framework_id, "percentile:ninety_nine_ninety", result[17])

      db.close
    end
  end
end

Client.run
