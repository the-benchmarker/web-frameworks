# frozen_string_literal: true

# Returns true if the command exists in PATH.
# Result is memoized to avoid repeated shell calls.
@command_cache = {}

def command_available?(cmd)
  @command_cache[cmd] ||= system("command -v #{cmd}", out: File::NULL, err: File::NULL)
end

def normalize_shell(shell)
  shell
    .gsub(/\\\s*\n/, " ") # escape newlines
    .gsub(/\s+/, " ") # collapse spaces
    .strip
end

class Hash
  def recursive_merge(hash)
    merge!(hash) { |_, old, new| old.instance_of?(Hash) ? old.recursive_merge(new) : new }
  end
end
