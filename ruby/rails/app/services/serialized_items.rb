class SerializedItems
  # This build-context copy matches .tasks/fixtures/serialization.json.
  # Parse and deeply freeze it once; render json: still encodes every response.
  # The baseline does not need to load the complete scenario's dataset.
  ITEMS = if Rails.configuration.x.complete
            JSON.parse(File.read(Rails.root.join('config/serialization.json')), freeze: true)
          else
            [].freeze
          end

  def self.call(seed:)
    { seed: seed, items: ITEMS }
  end
end
