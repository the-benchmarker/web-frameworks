class DeserializationSummary
  def self.call(seed:, items:)
    { seed: seed, count: items.length, idSum: items.sum { |item| item.fetch(:id) } }
  end
end
