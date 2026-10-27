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
    config = YAML.safe_load_file(path).except('specs')
    config['specs'] = version unless version.nil?
    allow(YAML).to receive(:safe_load_file).and_call_original
    allow(YAML).to receive(:safe_load_file).with(path).and_return(config)
  end

  it 'honors the explicit v1 selection for Sinatra' do
    commands = generated_commands('sinatra')

    expect(commands.fetch(:warmup).length).to eq(3)
    expect(commands.fetch(:warmup)).to all(include('/.load_scripts/default.lua'))
    expect(commands.fetch(:warmup)[0]).to include(':3000/ ')
    expect(commands.fetch(:warmup)[1]).to include(':3000/user/42 ')
    expect(commands.fetch(:warmup)[2]).to include(':3000/user ')
    expect(commands.fetch(:'collect-2').fetch(1).split(' && ')).to all(include('/.load_scripts/default.lua'))
    expect(commands.fetch(:test).first).to eq('ENGINE=puma LANGUAGE=ruby FRAMEWORK=sinatra bundle exec rspec')
  end

  it 'uses v2 workloads when the framework does not specify a version' do
    stub_framework_specs(nil)

    expect(generated_commands.fetch(:warmup).length).to eq(6)
  end

  it 'defaults to v2 workloads when no config layer specifies a version' do
    allow(YAML).to receive(:safe_load_file).and_wrap_original do |method, *args|
      method.call(*args).except('specs')
    end

    expect(generated_commands.fetch(:warmup).length).to eq(6)
  end

  it 'uses all six workloads for Rails' do
    commands = generated_commands

    expect(commands.fetch(:warmup).length).to eq(6)
    expect(commands.fetch(:warmup).first).to include('/.load_scripts/default.lua', ':3000/ ')
    expect(commands.fetch(:test).first).to eq('ENGINE=puma LANGUAGE=ruby FRAMEWORK=rails bundle exec rspec')
  end

  it 'honors an explicit v1 selection in the framework config' do
    stub_framework_specs('v1')

    expect(generated_commands.fetch(:warmup).length).to eq(3)
    expect(generated_commands.fetch(:test).first).to end_with('bundle exec rspec')
  end

  it 'rejects an unknown spec version before generating commands' do
    stub_framework_specs('v3')

    expect { generated_commands }.to raise_error(ArgumentError, 'specs must be v1 or v2')
  end

  it 'uses a separate Lua script for each workload without leaking it to the next route' do
    ENV['ROUTES'] = 'GET:/user/00000042,GET:/,POST:/deserialization,GET:/serialization,POST:/upload,POST:/user'
    requests = collected_requests

    %w[user_id default deserialization serialization upload default].each_with_index do |script, index|
      expect(requests.fetch(index)).to include("/.load_scripts/#{script}.lua")
    end
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

    expect(requests.grep(/--header|Accept-Encoding/)).to be_empty
    script_counts = { 'default' => 6, 'user_id' => 3, 'serialization' => 3, 'deserialization' => 3, 'upload' => 3 }
    script_counts.each do |script, count|
      expect(requests.count { |command| command.include?("/.load_scripts/#{script}.lua") }).to eq(count)
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
    expect(collected_requests.first).to include('/.load_scripts/default.lua')
  end

  it 'rejects malformed route entries before writing runnable commands' do
    ENV['ROUTES'] = 'deserialization'

    expect { generated_commands }.to raise_error(ArgumentError, %r{METHOD:/path})
  end
end
