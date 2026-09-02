# frozen_string_literal: true

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
