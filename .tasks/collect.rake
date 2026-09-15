require 'pg'
require 'yaml'
require 'json'

def insert_metric(db, framework_id, metric, value, concurrency_level_id)
  res = db.query('INSERT INTO keys (label) VALUES ($1) ON CONFLICT (label) DO UPDATE SET label = $1 RETURNING id', [metric])
  metric_id = res.first['id']

  res = db.query('INSERT INTO values (key_id, value) VALUES ($1, $2) RETURNING id', [metric_id, value])
  value_id = res.first['id']

  db.query('INSERT INTO metrics (value_id, framework_id, concurrency_id) VALUES ($1, $2, $3)', [value_id, framework_id, concurrency_level_id])
end

def upsert_framework(db, language, framework)
  res = db.query(
    'INSERT INTO languages (label) VALUES ($1) ON CONFLICT (label) DO UPDATE SET label = $1 RETURNING id', [language]
  )
  language_id = res.first['id']

  res = db.query(
    'INSERT INTO frameworks (language_id, label) VALUES ($1, $2) ON CONFLICT (language_id, label) DO UPDATE SET label = $2 RETURNING id',
    [language_id, framework]
  )
  res.first['id']
end

def upsert_concurrency(db, level)
  res = db.query(
    'INSERT INTO concurrencies (level) VALUES ($1) ON CONFLICT (level) DO UPDATE SET level = $1 RETURNING id', [level]
  )
  res.first['id']
end

task :collect do
  database = ENV.fetch('DATABASE_URL')
  db = PG.connect(database)

  # zrk --closed (see config.rake) sends each connection's next request the
  # instant its previous response completes, so achieved_rate is already the
  # framework's real max sustained throughput at this concurrency -- one file
  # per route, no picking among multiple runs needed. Under an open-loop ramp
  # (-R A:B) zrk >= 2.4.2 makes achieved_rate the last --interval only, so a
  # file from such a run is imported but flagged instead of silently ranked
  # on one second of data.
  Dir.glob('*/*/.results/*/**.json').each do |file|
    next if File.basename(file) == 'memory.json'
    next if File.basename(file) == 'memory_idle.json'
    next if File.basename(file) == 'saturation.json'

    pp file

    language, framework, _, concurrency = file.split('/')

    framework_id = upsert_framework(db, language, framework)
    concurrency_level_id = upsert_concurrency(db, concurrency)

    data = YAML.safe_load_file(file, symbolize_names: true)

    if data.dig(:config, :closed) == false
      warn "#{file}: produced by an open-loop zrk run (config.closed=false); " \
           'achieved_rate covers only the final interval. Re-run `rake config` and collect again.'
    end

    results = {
      duration_ms: data[:duration_s] * 1000,
      total_requests: data[:requests],
      total_requests_per_s: data[:achieved_rate],
      total_bytes_received: data[:bytes],
      socket_connection_errors: data.dig(:errors, :connect),
      socket_read_errors: data.dig(:errors, :read),
      socket_write_errors: data.dig(:errors, :write),
      http_errors: data.dig(:errors, :non_2xx_3xx),
      request_timeouts: data.dig(:errors, :timeout),
      minimum_latency: data.dig(:latency_us, :min) / 1_000_000.0,
      average_latency: data.dig(:latency_us, :mean) / 1_000_000.0,
      standard_deviation: data.dig(:latency_us, :stdev) / 1_000_000.0,
      percentile50: data.dig(:latency_us, :p50) / 1_000_000.0,
      percentile75: data.dig(:latency_us, :p75) / 1_000_000.0,
      percentile90: data.dig(:latency_us, :p90) / 1_000_000.0,
      percentile99: data.dig(:latency_us, :p99) / 1_000_000.0,
      percentile99999: data.dig(:latency_us, :p99_99) / 1_000_000.0
    }

    results.each do |key, value|
      insert_metric(db, framework_id, key, value, concurrency_level_id)
    end
  end

  # Import idle memory (concurrency level 0 = pre-load baseline)
  Dir.glob('*/*/.results/memory_idle.json').each do |file|
    language, framework = file.split('/')

    framework_id = upsert_framework(db, language, framework)
    concurrency_level_id = upsert_concurrency(db, 0)

    data = JSON.load_file(file, symbolize_names: true)
    insert_metric(db, framework_id, :memory_idle_bytes, data[:idle_bytes], concurrency_level_id)
  end

  # Import measurement validity: how much of its allotted CPU the SERVER
  # actually burned during the run. A low value means the server was never the
  # constraint, so the throughput number describes whatever was - most often the
  # load generator - rather than the framework.
  Dir.glob('*/*/.results/*/saturation.json').each do |file|
    language, framework, _, concurrency = file.split('/')

    data = JSON.load_file(file, symbolize_names: true)
    next unless data[:saturation]

    framework_id = upsert_framework(db, language, framework)
    concurrency_level_id = upsert_concurrency(db, concurrency)

    insert_metric(db, framework_id, :server_cpu_saturation, data[:saturation], concurrency_level_id)
  end

  # Import per-concurrency memory (peak + average under load)
  Dir.glob('*/*/.results/*/memory.json').each do |file|
    language, framework, _, concurrency = file.split('/')

    framework_id = upsert_framework(db, language, framework)
    concurrency_level_id = upsert_concurrency(db, concurrency)

    data = JSON.load_file(file, symbolize_names: true)
    insert_metric(db, framework_id, :memory_peak_bytes, data[:peak_bytes], concurrency_level_id)
    insert_metric(db, framework_id, :memory_average_bytes, data[:average_bytes], concurrency_level_id)
  end

  db.close
end
