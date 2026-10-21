require 'rake'
require_relative 'spec_helper'
require_relative '../.tasks/helpers'

load File.join(HttpHelpers::ROOT, '.tasks/config.rake') unless defined?(MANIFESTS)

RSpec.describe 'benchmark request generation' do
  around do |example|
    original = ENV.to_h
    {
      'ROUTES' => nil, 'LATENCY_RATE' => nil, 'LOAD_CPUS' => nil,
      'SERVER_CPUS' => nil, 'THREADS' => '1', 'CONCURRENCIES' => '2', 'DURATION' => '1s'
    }.each { |key, value| ENV[key] = value }
    Dir.chdir(HttpHelpers::ROOT) { example.run }
  ensure
    ENV.replace(original)
  end

  def generated_commands(framework = 'rails')
    commands_for('ruby', framework, 'puma').fetch(:commands)
  end

  def collected_requests
    generated_commands.fetch(:'collect-2').fetch(1).split(' && ')
  end

  def stub_framework_specs(version)
    path = File.join(HttpHelpers::ROOT, 'ruby', 'rails', 'config.yaml')
    config = YAML.safe_load_file(path).merge('specs' => version)
    allow(YAML).to receive(:safe_load_file).and_call_original
    allow(YAML).to receive(:safe_load_file).with(path).and_return(config)
  end

  it 'uses v1 workloads by default' do
    commands = generated_commands('sinatra')

    expect(commands.fetch(:warmup).length).to eq(1)
    expect(commands.fetch(:warmup).first).to end_with(':3000/')
    expect(commands.fetch(:test).first).to eq('ENGINE=puma LANGUAGE=ruby FRAMEWORK=sinatra bundle exec rspec')
  end

  it 'uses all six workloads for Rails' do
    commands = generated_commands

    expect(commands.fetch(:warmup).length).to eq(6)
    expect(commands.fetch(:warmup).first).to end_with(':3000/')
    expect(commands.fetch(:test).first).to eq('ENGINE=puma LANGUAGE=ruby FRAMEWORK=rails bundle exec rspec')
  end

  it 'honors an explicit v1 selection in the framework config' do
    stub_framework_specs('v1')

    expect(generated_commands.fetch(:warmup).length).to eq(1)
    expect(generated_commands.fetch(:test).first).to end_with('bundle exec rspec')
  end

  it 'rejects an unknown spec version before generating commands' do
    stub_framework_specs('v3')

    expect { generated_commands }.to raise_error(ArgumentError, 'specs must be v1 or v2')
  end

  it 'uses a separate Lua script for each workload without leaking it to the next route' do
    ENV['ROUTES'] = 'GET:/user/00000042,GET:/,POST:/deserialization,GET:/serialization,POST:/upload,POST:/user'
    requests = collected_requests

    %w[user_id deserialization serialization upload].zip([0, 2, 3, 4]).each do |script, index|
      expect(requests.fetch(index)).to include("/.load_scripts/#{script}.lua")
    end
    [1, 5].each { |index| expect(requests.fetch(index)).not_to include('--script') }
  end

  it 'injects both shared fixtures through read-only runtime mounts' do
    container = generated_commands.fetch(:build).find { |command| command.start_with?('docker run ') }

    %w[deserialization.json upload.bin].each do |filename|
      expect(container).to include("--mount type=bind,source=\"$$(pwd)/#{filename}\",target=/#{filename},readonly")
    end
    expect(container).not_to include('target=/serialization.json,', 'SERIALIZATION_FIXTURE')
  end

  it 'supplies the repository location to Lua when the generator uses an absolute script path' do
    ENV['ROUTES'] = 'POST:/deserialization'

    expect(collected_requests.first).to end_with(HttpHelpers::ROOT)
  end

  it 'uses the same bodies and headers for warmup and both measurement passes' do
    ENV['LATENCY_RATE'] = '50%'
    commands = generated_commands
    requests = commands.fetch(:warmup) + commands.fetch(:'collect-2').fetch(1).split(' && ')

    expect(requests).to all(include('--header Accept-Encoding:identity'))
    %w[serialization deserialization upload].each do |script|
      expect(requests.count { |command| command.include?("/.load_scripts/#{script}.lua") }).to eq(3)
    end
  end

  it 'allows a route override without changing the selected specs' do
    ENV['ROUTES'] = 'POST:/upload'

    expect(generated_commands.fetch(:warmup).length).to eq(1)
    expect(collected_requests.first).to include('/.load_scripts/upload.lua')
    expect(generated_commands.fetch(:test).first).to end_with('bundle exec rspec')
  end

  it 'does not change the request method when a matching path has another method' do
    ENV['ROUTES'] = 'GET:/upload'

    expect(collected_requests.first).to include('--method GET')
    expect(collected_requests.first).not_to include('--script')
  end

  it 'rejects malformed route entries before writing runnable commands' do
    ENV['ROUTES'] = 'deserialization'

    expect { generated_commands }.to raise_error(ArgumentError, %r{METHOD:/path})
  end
end
