class SerializedItems
  def self.call(count:, seed:)
    { items: Array.new(count) { |index| { id: index, value: "#{seed}:#{index}" } } }
  end
end
