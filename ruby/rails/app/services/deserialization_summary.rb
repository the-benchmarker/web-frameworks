class DeserializationSummary
  def self.call(items:)
    { count: items.length, checksum: items.sum { |item| item.fetch('id').to_i } }
  end
end
