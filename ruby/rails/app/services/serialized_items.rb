class SerializedItems
  # The runner mounts the shared fixture into the container read-only.
  # Parse and deeply freeze it once; render json: still encodes every response.
  # The baseline does not need to load the complete scenario's dataset.
  ITEMS = if Rails.configuration.x.complete
            path = ENV.fetch('SERIALIZATION_FIXTURE') { Rails.root.join('..', '..', 'serialization.json') }
            JSON.parse(File.read(path), freeze: true)
          else
            [].freeze
          end

  def self.call(seed:)
    { seed: seed, items: ITEMS }
  end
end
