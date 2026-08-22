# frozen_string_literal: true

# Measurement-validity probe.
#
# A benchmark number only means what it claims if the SERVER was the thing that
# ran out of capacity. When the load generator saturates first, every framework
# converges on the generator's ceiling and the board ends up ranking entries by
# how much CPU they left over for it - which is not a property of the framework.
#
# This brackets a collect run and records how much CPU the server container
# actually burned, as a fraction of the cores it was given. Two reads of the
# container's cgroup counter, so it adds no sampling load of its own to the
# host it is trying to measure.
#
#   ruby .tasks/saturation.rb --cid <cidfile> --start --state <f>
#   <oha runs>
#   ruby .tasks/saturation.rb --cid <cidfile> --stop  --state <f> --out <json> --cores N

require 'json'
require 'optparse'
require 'fileutils'
require 'etc'

options = {}
OptionParser.new do |opts|
  opts.on('--cid PATH', 'Path to container ID file') { |v| options[:cid] = v }
  opts.on('--state PATH', 'Path to the bracket state file') { |v| options[:state] = v }
  opts.on('--out PATH', 'Where to write the result (stop only)') { |v| options[:out] = v }
  opts.on('--cores N', Float, 'Cores the server container was given') { |v| options[:cores] = v }
  opts.on('--start', 'Record the opening counter') { options[:start] = true }
  opts.on('--stop', 'Record the closing counter and emit the result') { options[:stop] = true }
end.parse!

raise '--cid is required' unless options[:cid]
raise '--state is required' unless options[:state]
raise 'one of --start / --stop is required' unless options[:start] || options[:stop]

CONTAINER_ID = File.read(options[:cid]).strip

# Cumulative CPU microseconds burned by the container, or nil when the counter
# cannot be read (cgroup layout differences, container already gone). A probe
# that cannot measure must not fabricate a verdict - callers treat nil as
# "unknown" rather than as zero.
def cpu_usec
  v2 = `docker exec #{CONTAINER_ID} cat /sys/fs/cgroup/cpu.stat 2>/dev/null`
  if (m = v2.match(/^usage_usec\s+(\d+)/))
    return m[1].to_i
  end

  # cgroup v1 reports nanoseconds in a different file.
  v1 = `docker exec #{CONTAINER_ID} cat /sys/fs/cgroup/cpuacct/cpuacct.usage 2>/dev/null`.strip
  return v1.to_i / 1000 if v1.match?(/\A\d+\z/)

  nil
end

# Cores the container can actually use. Reading this from the HOST would be
# wrong whenever the container is constrained - by --cpuset-cpus, by a CPU
# quota, or simply by the VM the daemon runs in - and an inflated denominator
# makes a busy server look idle. Ask the container itself.
def container_cores
  # A hard quota (--cpus) is the tightest bound when present.
  quota = `docker exec #{CONTAINER_ID} cat /sys/fs/cgroup/cpu.max 2>/dev/null`.strip
  if (m = quota.match(/\A(\d+)\s+(\d+)\z/))
    return m[1].to_f / m[2].to_f
  end

  # Otherwise the CPUs visible to the container (honours --cpuset-cpus).
  n = `docker exec #{CONTAINER_ID} nproc 2>/dev/null`.strip
  return n.to_f if n.match?(/\A\d+\z/)

  nil
end

if options[:start]
  FileUtils.mkdir_p(File.dirname(options[:state]))
  File.write(options[:state], JSON.generate({ usec: cpu_usec, at: Time.now.to_f }))
  exit 0
end

raise '--out is required with --stop' unless options[:out]

state = JSON.parse(File.read(options[:state]), symbolize_names: true)
after = cpu_usec
before = state[:usec]
wall = Time.now.to_f - state[:at]
cores = options[:cores] || container_cores || Etc.nprocessors.to_f

result =
  if before.nil? || after.nil? || wall <= 0
    { saturation: nil, verdict: 'unknown', reason: 'cgroup CPU counter unavailable' }
  else
    cpu_seconds = (after - before) / 1_000_000.0
    capacity = wall * cores
    saturation = capacity.positive? ? cpu_seconds / capacity : nil

    # A server that never got near the capacity it was given did not limit this
    # run - something else did, and the number describes that something else.
    verdict =
      if saturation.nil? then 'unknown'
      elsif saturation >= 0.75 then 'server-bound'
      elsif saturation >= 0.50 then 'mixed'
      else 'server-idle'
      end

    {
      server_cpu_seconds: cpu_seconds.round(3),
      wall_seconds: wall.round(3),
      cores: cores,
      saturation: saturation&.round(4),
      verdict: verdict
    }
  end

FileUtils.mkdir_p(File.dirname(options[:out]))
File.write(options[:out], JSON.generate(result))

if result[:verdict] == 'server-idle'
  warn "[saturation] server used #{(result[:saturation] * 100).round(1)}% of its allotted CPU " \
       "(#{result[:server_cpu_seconds]}s across #{result[:cores]} cores over #{result[:wall_seconds]}s). " \
       'The server was not the bottleneck - this measurement describes the load generator, not the framework.'
end
