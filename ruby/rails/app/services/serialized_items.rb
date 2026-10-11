class SerializedItems
  # The generator copies the shared fixture into the Rails build context.
  # Freeze the complete data structure so concurrent requests cannot mutate it.
  # The baseline can boot without preparing the complete scenario's fixture.
  ITEMS = if Rails.configuration.x.complete
            JSON.parse(File.read(Rails.root.join('serialization.json')), freeze: true)
          else
            [].freeze
          end

  def self.call(seed:)
    { seed: seed, items: ITEMS }
  end
end
