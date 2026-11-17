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

RSpec.describe 'cpuset_size' do
  it 'counts single cpus and ranges' do
    expect(cpuset_size('0-3,8,10-11')).to eq(7)
  end

  it 'is nil when unset' do
    expect(cpuset_size(nil)).to be_nil
    expect(cpuset_size('')).to be_nil
  end

  it 'is nil for a spec it cannot count' do
    expect(cpuset_size('0-')).to be_nil
    expect(cpuset_size('a')).to be_nil
  end
end

RSpec.describe 'latency_rate_for' do
  it 'takes a percentage of the closed-loop rate' do
    expect(latency_rate_for('50%', 167_504.2)).to eq(83_752)
    expect(latency_rate_for('12.5%', 80_000)).to eq(10_000)
  end

  it 'never asks for less than one request per second' do
    expect(latency_rate_for('1%', 20)).to eq(1)
  end

  it 'passes an absolute rate through unchanged' do
    expect(latency_rate_for('20000', nil)).to eq(20_000)
    expect(latency_rate_for(' 20000 ', 5)).to eq(20_000)
  end

  it 'is nil for a percentage without a closed-loop rate, and for nonsense' do
    expect(latency_rate_for('50%', nil)).to be_nil
    expect(latency_rate_for('50%', 0)).to be_nil
    expect(latency_rate_for('fast', 100)).to be_nil
    expect(latency_rate_for('0', 100)).to be_nil
  end
end
