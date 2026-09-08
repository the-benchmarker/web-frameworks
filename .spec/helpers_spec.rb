require_relative '../.tasks/helpers'

RSpec.describe Hash do
  describe '#recursive_merge' do
    it 'leaves the receiver alone' do
      main = { 'framework' => { 'version' => 1 } }
      main.recursive_merge('framework' => { 'website' => 'first.example' })

      expect(main).to eq('framework' => { 'version' => 1 })
    end

    # .tasks/db.rake merges into the same main config once per framework, so a
    # value read here has to come from the hash passed in and from nothing else.
    it 'does not carry a value over to the next merge' do
      main = {}
      first = main.recursive_merge('framework' => { 'website' => 'first.example' })
      second = main.recursive_merge('framework' => { 'version' => 2 })

      expect(first.dig('framework', 'website')).to eq('first.example')
      expect(second.dig('framework', 'website')).to be_nil
    end

    it 'merges nested hashes' do
      config = { 'framework' => { 'version' => 1, 'engines' => { 'node' => 'a' } } }

      expect(config.recursive_merge('framework' => { 'engines' => { 'node' => 'b', 'bun' => 'c' } }))
        .to eq('framework' => { 'version' => 1, 'engines' => { 'node' => 'b', 'bun' => 'c' } })
    end
  end
end
