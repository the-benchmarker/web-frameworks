class SerializedItems
  def self.call(seed:)
    Array.new(100) { { seed: seed } }
  end
end
