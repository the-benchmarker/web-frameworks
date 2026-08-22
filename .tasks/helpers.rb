# frozen_string_literal: true

# Returns true if the command exists in PATH.
# Result is memoized to avoid repeated shell calls.
@command_cache = {}

def command_available?(cmd)
  @command_cache[cmd] ||= system("command -v #{cmd}", out: File::NULL, err: File::NULL)
end

# Prefix that pins the load generator to a CPU set so it cannot contend with the
# server under test. Empty string when LOAD_CPUS is unset, which leaves the
# command byte-identical to the unpinned form.
def load_generator_prefix(load_cpus)
  return '' unless load_cpus

  # NB: not command_available? - that helper shells out without a shell, so it
  # cannot see builtins and always reports false.
  unless system('sh', '-c', 'command -v taskset', out: File::NULL, err: File::NULL)
    warn "LOAD_CPUS=#{load_cpus} was set but taskset is not available; load generator will NOT be pinned."
    return ''
  end

  "taskset -c #{load_cpus} "
end

def normalize_shell(shell)
  shell
    .gsub(/\\\s*\n/, ' ') # escape newlines
    .gsub(/\s+/, ' ') # collapse spaces
    .strip
end

class Hash
  def recursive_merge(hash)
    merge!(hash) { |_, old, new| old.instance_of?(Hash) ? old.recursive_merge(new) : new }
  end
end
