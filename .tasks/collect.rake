require 'pg'
require 'yaml'
require 'json'

# Cache dimension IDs for this import and write a file's values and metric
# links together, avoiding three database round trips for every measurement.
class CollectDatabase
  def initialize(db)
    @db = db
    @languages = {}
    @frameworks = {}
    @concurrencies = {}
    @keys = {}
  end

  def insert_metrics(framework_id, results, concurrency_id)
    params = [framework_id, concurrency_id]
    rows = results.map do |metric, value|
      key_id = @keys[metric] ||= @db.query(
        'INSERT INTO keys (label) VALUES ($1) ON CONFLICT (label) DO UPDATE SET label = $1 RETURNING id', [metric]
      ).first['id']
      params.push(key_id, value)
      "($#{params.length - 1}::bigint, $#{params.length}::numeric)"
    end

    @db.query(<<~SQL, params)
      WITH inserted_values AS (
        INSERT INTO values (key_id, value) VALUES #{rows.join(', ')}
        RETURNING id
      )
      INSERT INTO metrics (value_id, framework_id, concurrency_id)
      SELECT id, $1::bigint, $2::bigint FROM inserted_values
    SQL
  end

  def insert_metric(framework_id, metric, value, concurrency_id)
    insert_metrics(framework_id, { metric => value }, concurrency_id)
  end

  def upsert_framework(language, framework)
    @frameworks[[language, framework]] ||= begin
      language_id = @languages[language] ||= @db.query(
        'INSERT INTO languages (label) VALUES ($1) ON CONFLICT (label) DO UPDATE SET label = $1 RETURNING id', [language]
      ).first['id']

      @db.query(
        'INSERT INTO frameworks (language_id, label) VALUES ($1, $2) ON CONFLICT (language_id, label) DO UPDATE SET label = $2 RETURNING id',
        [language_id, framework]
      ).first['id']
    end
  end

  def upsert_concurrency(level)
    @concurrencies[level] ||= @db.query(
      'INSERT INTO concurrencies (level) VALUES ($1) ON CONFLICT (level) DO UPDATE SET level = $1 RETURNING id', [level]
    ).first['id']
  end
end

task :collect do
  database = ENV.fetch('DATABASE_URL')
  db = PG.connect(database)
  collector = CollectDatabase.new(db)

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
    next if File.basename(file).end_with?('_latency.json')

    pp file

    language, framework, _, concurrency = file.split('/')

    framework_id = collector.upsert_framework(language, framework)
    concurrency_level_id = collector.upsert_concurrency(concurrency)

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

    collector.insert_metrics(framework_id, results, concurrency_level_id)
  end

  # Import idle memory (concurrency level 0 = pre-load baseline)
  Dir.glob('*/*/.results/memory_idle.json').each do |file|
    language, framework = file.split('/')

    framework_id = collector.upsert_framework(language, framework)
    concurrency_level_id = collector.upsert_concurrency(0)

    data = JSON.load_file(file, symbolize_names: true)
    collector.insert_metric(framework_id, :memory_idle_bytes, data[:idle_bytes], concurrency_level_id)
  end

  # Import measurement validity: how much of its allotted CPU the SERVER
  # actually burned during the run. A low value means the server was never the
  # constraint, so the throughput number describes whatever was - most often the
  # load generator - rather than the framework.
  Dir.glob('*/*/.results/*/saturation.json').each do |file|
    language, framework, _, concurrency = file.split('/')

    data = JSON.load_file(file, symbolize_names: true)
    next unless data[:saturation]

    framework_id = collector.upsert_framework(language, framework)
    concurrency_level_id = collector.upsert_concurrency(concurrency)

    collector.insert_metric(framework_id, :server_cpu_saturation, data[:saturation], concurrency_level_id)
    # Absolute cores kept busy, so a single-threaded server (a low share of
    # many allotted cores, one core pegged) can be told apart from an idle one.
    if data[:cores_used]
      collector.insert_metric(framework_id, :server_cpu_cores, data[:cores_used], concurrency_level_id)
    end
  end

  # Fixed-rate latency pass (LATENCY_RATE at `rake config`): the closed-loop
  # run finds the ceiling; this one reports latency at a defined load,
  # coordinated-omission corrected. Its rates are metrics of their own and
  # never fold into total_requests_per_s. rate_ratio < 1 means the framework
  # could not sustain the target, and the latency then includes the backlog.
  Dir.glob('*/*/.results/*/*_latency.json').each do |file|
    language, framework, _, concurrency = file.split('/')

    data = JSON.load_file(file, symbolize_names: true)
    next unless data[:latency_us]

    framework_id = collector.upsert_framework(language, framework)
    concurrency_level_id = collector.upsert_concurrency(concurrency)

    results = {
      latency_at_rate_target_rps: data[:target_rate],
      latency_at_rate_achieved_rps: data[:achieved_rate],
      latency_at_rate_ratio: data[:rate_ratio],
      latency_at_rate_average: data.dig(:latency_us, :mean) / 1_000_000.0,
      latency_at_rate_percentile50: data.dig(:latency_us, :p50) / 1_000_000.0,
      latency_at_rate_percentile99: data.dig(:latency_us, :p99) / 1_000_000.0
    }
    collector.insert_metrics(framework_id, results, concurrency_level_id)
  end

  # Import per-concurrency memory (peak + average under load)
  Dir.glob('*/*/.results/*/memory.json').each do |file|
    language, framework, _, concurrency = file.split('/')

    framework_id = collector.upsert_framework(language, framework)
    concurrency_level_id = collector.upsert_concurrency(concurrency)

    data = JSON.load_file(file, symbolize_names: true)
    results = {
      memory_peak_bytes: data[:peak_bytes],
      memory_average_bytes: data[:average_bytes]
    }
    collector.insert_metrics(framework_id, results, concurrency_level_id)
  end

  db.close
end
