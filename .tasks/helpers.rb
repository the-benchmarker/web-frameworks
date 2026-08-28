# frozen_string_literal: true

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
