class SerializedItems
  def self.call(seed:)
    100.times.map { { seed: seed } }
  end
end
