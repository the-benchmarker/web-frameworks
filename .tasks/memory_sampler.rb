# frozen_string_literal: true

require 'fileutils'
require 'json'
require 'optparse'

options = {}
OptionParser.new do |opts|
  opts.on('--cid PATH', 'Path to container ID file') { |v| options[:cid] = v }
  opts.on('--out PATH', 'Path to output JSON file') { |v| options[:out] = v }
  opts.on('--idle', 'Take a single idle snapshot instead of continuous sampling') { options[:idle] = true }
end.parse!

raise '--cid is required' unless options[:cid]
raise '--out is required' unless options[:out]

container_id = File.read(options[:cid]).strip

UNITS = {
  'B'   => 1,
  'kB'  => 1_000,
  'MB'  => 1_000_000,
  'GB'  => 1_000_000_000,
  'TB'  => 1_000_000_000_000,
  'KiB' => 1_024,
  'MiB' => 1_024 ** 2,
  'GiB' => 1_024 ** 3,
  'TiB' => 1_024 ** 4
}.freeze

def parse_memory(mem_usage)
  # MemUsage format: "45.3MiB / 7.6GiB" — we only want the left side
  used = mem_usage.split('/').first.strip
  match = used.match(/^([\d.]+)\s*([A-Za-z]+)$/)
  return 0 unless match

  (match[1].to_f * (UNITS[match[2]] || 1)).round
end

def sample_memory(container_id)
  raw = `docker stats --no-stream --format '{{json .}}' #{container_id} 2>/dev/null`.strip
  return nil if raw.empty?

  data = JSON.parse(raw)
  parse_memory(data['MemUsage'])
rescue JSON::ParserError
  nil
end

FileUtils.mkdir_p(File.dirname(options[:out]))

if options[:idle]
  bytes = sample_memory(container_id) || 0
  File.write(options[:out], JSON.generate({ idle_bytes: bytes }))
else
  samples = []
  running = true

  trap('TERM') { running = false }
  trap('INT')  { running = false }

  while running
    value = sample_memory(container_id)
    samples << value if value
    sleep 0.5
  end

  peak    = samples.max || 0
  average = samples.empty? ? 0 : (samples.sum.to_f / samples.size).round

  File.write(options[:out], JSON.generate({ peak_bytes: peak, average_bytes: average, samples: samples.size }))
end
