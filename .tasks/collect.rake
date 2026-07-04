require 'pg'
require 'yaml'

PIPELINE = {
  GET: File.join(Dir.pwd, 'pipeline.lua'),
  POST: File.join(Dir.pwd, 'pipeline_post.lua')
}.freeze

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

  Dir.glob('*/*/.results/*/**.json').each do |file|
    next if File.basename(file) == 'memory.json'
    next if File.basename(file) == 'memory_idle.json'

    pp file

    language, framework, _, concurrency = file.split('/')

    framework_id = upsert_framework(db, language, framework)
    concurrency_level_id = upsert_concurrency(db, concurrency)

    data = YAML.safe_load_file(file, symbolize_names: true)

    results = {
      duration_ms: data.dig(:summary, :total) * 1000,
      total_requests: -1,
      total_requests_per_s: data.dig(:summary, :requestsPerSec),
      total_bytes_received: data.dig(:summary, :totalData),
      socket_connection_errors: -1,
      socket_read_errors: -1,
      socket_write_errors: -1,
      http_errors: -1,
      request_timeouts: -1,
      minimum_latency: -1,
      average_latency: -1,
      standard_deviation: -1,
      percentile50: data.dig(:latencyPercentiles, :p50),
      percentile75: data.dig(:latencyPercentiles, :p75),
      percentile90: data.dig(:latencyPercentiles, :p90),
      percentile99: data.dig(:latencyPercentiles, :p95),
      percentile99999: -1
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
