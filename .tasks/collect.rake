require 'open3'
require 'csv'
require 'etc'
require 'bigdecimal/util'

PIPELINE = {
  GET: File.join(Dir.pwd, 'pipeline.lua'),
  POST: File.join(Dir.pwd, 'pipeline_post.lua')
}.freeze

def insert(db, framework_id, metric, value, concurrency_level_id)
  res = db.query('INSERT INTO keys (label) VALUES ($1) ON CONFLICT (label) DO UPDATE SET label = $1 RETURNING id',
                 [metric])

  metric_id = res.first['id']

  res = db.query('INSERT INTO values (key_id, value) VALUES ($1, $2) RETURNING id', [metric_id, value])
  value_id = res.first['id']

  db.query('INSERT INTO metrics (value_id, framework_id, concurrency_id) VALUES ($1, $2, $3)',
           [value_id, framework_id, concurrency_level_id])
end

task :collect do
  database = ENV.fetch('DATABASE_URL')
  db = PG.connect(database)
 Dir.glob('*/*/.results/*/**.json').each do |file|
  info = file.split('/')
   language = info[0]
   framework = info[1]
   concurrency = info[3]
  res = db.query(
    'INSERT INTO languages (label) VALUES ($1) ON CONFLICT (label) DO UPDATE SET label = $1 RETURNING id', [language]
  )
  language_id = res.first['id']

  res = db.query(
    'INSERT INTO frameworks (language_id, label) VALUES ($1, $2) ON CONFLICT (language_id, label) DO UPDATE SET label = $2 RETURNING id', [
      language_id, framework
    ]
  )
  framework_id = res.first['id']
  data = YAML.load_file(file, symbolize_names: true)

      res = db.query(
        'INSERT INTO concurrencies (level) VALUES ($1) ON CONFLICT (level) DO UPDATE SET level = $1 RETURNING id', [concurrency]
      )

      concurrency_level_id = res.first['id']
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
  percentile99:data.dig(:latencyPercentiles, :p95),
  'percentile99.999': -1,
}
results.each do |key, value|
  insert(db, framework_id, key, value, concurrency_level_id)
    end
end
  db.close
end
