# frozen_string_literal: true

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
