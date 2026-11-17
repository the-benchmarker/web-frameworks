# frozen_string_literal: true

require 'etc'

# Prefix that pins the load generator to a CPU set so it cannot contend with the
# server under test. Empty string when LOAD_CPUS is unset, which leaves the
# command byte-identical to the unpinned form.
#
# The probe runs through a real shell: `command` is a builtin, so system() with
# a single string would exec a binary named "command" and always report false.
def load_generator_prefix(load_cpus)
  return '' unless load_cpus

  unless system('sh', '-c', 'command -v taskset', out: File::NULL, err: File::NULL)
    warn "LOAD_CPUS=#{load_cpus} was set but taskset is not available; load generator will NOT be pinned."
    return ''
  end

  "taskset -c #{load_cpus} "
end

# Number of CPUs named by a cpuset spec such as "0-3,8" (the syntax of both
# --cpuset-cpus and taskset -c). nil when the spec is unset or unreadable, so
# the caller can fall back rather than pin a wrong thread count.
def cpuset_size(spec)
  return nil if spec.nil? || spec.strip.empty?

  spec.split(',').sum do |part|
    first, last = part.strip.split('-', 2)
    return nil unless first&.match?(/\A\d+\z/) && (last.nil? || last.match?(/\A\d+\z/))

    last ? last.to_i - first.to_i + 1 : 1
  end
end

# The -R of the fixed-rate latency pass (LATENCY_RATE). "N%" is N percent of
# the closed-loop achieved_rate handed in, nil when that run left no result
# (the caller decides what to do); a plain integer is an absolute rate, the
# same for every framework. Anything else is nil.
def latency_rate_for(spec, achieved)
  spec = spec.to_s.strip
  if (m = spec.match(/\A(\d+(?:\.\d+)?)%\z/))
    return nil unless achieved.is_a?(Numeric) && achieved.positive?

    return [(achieved * m[1].to_f / 100.0).floor, 1].max
  end
  return spec.to_i if spec.match?(/\A\d+\z/) && spec.to_i.positive?

  nil
end

# Total RAM in kB from the text of /proc/meminfo, nil when it has no MemTotal.
def memory_kb_from(meminfo)
  meminfo.to_s[/^MemTotal:\s+(\d+)\s*kB/, 1]&.to_i
end

# CPU model from the text of /proc/cpuinfo. x86 kernels print "model name";
# nil where the kernel does not (arm64), so the caller can fall back.
def cpu_name_from(cpuinfo)
  cpuinfo.to_s[/^model name\s*:\s*(.+)$/, 1]&.strip
end

# What the machine exporting the results actually is, for data.json's hardware
# block. Linux reads /proc; macOS asks sysctl. The architecture stands in for a
# CPU name the kernel does not report.
def host_hardware
  read = ->(path) { File.exist?(path) ? File.read(path) : '' }
  sysctl = ->(key) { `sysctl -n #{key} 2>/dev/null`.strip }
  linux = File.exist?('/proc/meminfo')

  memory = linux ? memory_kb_from(read.call('/proc/meminfo')) : sysctl.call('hw.memsize').to_i / 1024
  cpu_name = linux ? cpu_name_from(read.call('/proc/cpuinfo')) : sysctl.call('machdep.cpu.brand_string')
  cpu_name = Etc.uname[:machine] if cpu_name.nil? || cpu_name.empty?

  { cpus: Etc.nprocessors, memory: memory, cpu_name: cpu_name, os: Etc.uname }
end

def normalize_shell(shell)
  shell
    .gsub(/\\\s*\n/, " ") # escape newlines
    .gsub(/\s+/, " ") # collapse spaces
    .strip
end

class Hash
  # Returns a new hash and leaves the receiver alone. With `merge!` the receiver
  # kept every key it was ever merged with, so in .tasks/db.rake, where the same
  # main config is merged once per framework, a framework whose config.yaml does
  # not set a key got the value of the framework merged before it.
  def recursive_merge(hash)
    merge(hash) { |_, old, new| old.instance_of?(Hash) ? old.recursive_merge(new) : new }
  end
end
