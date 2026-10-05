require "digest"

class DeserializationSummary
  def self.call(items:)
    values = items.map { |item| item.fetch(:value) }
    { count: values.length, checksum: Digest::SHA256.hexdigest(values.join("\n")) }
  end
end
