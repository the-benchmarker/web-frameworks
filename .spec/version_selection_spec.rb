require 'fileutils'
require 'open3'
require 'tmpdir'
require_relative 'spec_helper'

RSpec.describe 'benchmark contract selection' do
  def selected_examples(version, environment_version: nil)
    Dir.mktmpdir('benchmark-specs') do |root|
      FileUtils.cp_r(File.join(HttpHelpers::ROOT, '.spec'), root)
      framework = File.join(root, 'ruby', 'example')
      FileUtils.mkdir_p(framework)
      File.write(File.join(framework, 'config.yaml'), YAML.dump(version.nil? ? {} : { 'specs' => version }))
      env = { 'LANGUAGE' => 'ruby', 'FRAMEWORK' => 'example', 'BENCHMARK_VERSION' => environment_version }
      output, error, status = Open3.capture3(env, 'bundle', 'exec', 'rspec',
                                             File.join(root, '.spec/v1'), File.join(root, '.spec/v2'),
                                             '--dry-run', '--format', 'json')
      expect(status.success?).to be(true), error + output
      JSON.parse(output).fetch('examples')
    end
  end

  it 'runs every v2-only example when specs is omitted, regardless of environment override' do
    examples = selected_examples(nil, environment_version: 'v1')
    files = examples.map { |example| File.basename(example.fetch('file_path')) }.uniq

    expect(files).to contain_exactly('routing_spec.rb',
                                     'serialization_spec.rb', 'deserialization_spec.rb', 'upload_spec.rb')
  end

  it 'runs every v1-only example for specs: v1, even with a v2 environment override' do
    version = 'v1'
    examples = selected_examples(version, environment_version: 'v2')

    expect(examples).not_to be_empty
    expect(examples.map { |example| example.fetch('file_path') }).to all(include('/.spec/v1/'))
    expect(examples.map { |example| example.fetch('description') }).to include('returns the plain route ID 42')
  end

  it 'runs every v2 endpoint when specs: v2 is defined' do
    examples = selected_examples('v2', environment_version: 'v1')
    files = examples.map { |example| File.basename(example.fetch('file_path')) }.uniq

    expect(files).to contain_exactly('routing_spec.rb',
                                     'serialization_spec.rb', 'deserialization_spec.rb', 'upload_spec.rb')
    expect(examples.map { |example| example.fetch('description') }).not_to include('returns the plain route ID 42')
  end
end
