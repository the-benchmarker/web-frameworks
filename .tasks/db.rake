require 'pg'
require 'mustache'
require 'yaml'
require 'active_support/number_helper'
require 'dotenv'
require 'etc'

Dotenv.load

MAP = {
  duration_ms: [],
  total_requests: [],
  total_requests_per_s: [:summary, :requestsPerSec],
  total_bytes_received: [],
  socket_connection_errors: [],
  socket_read_errors: [],
  socket_write_errors: [],
  http_errors: [],
  request_timeouts: [],
  minimum_latency: [],
  maximum_latency: [],
  average_latency: [],
 standard_deviation: [],
 percentile_50: [:latencyPercentiles, :p50],
 percentile_75: [:latencyPercentiles, :p75],
percentile_90: [:latencyPercentiles,  :p90],
percentile_99: [:latencyPercentiles, :p99],
'percentile_99.999'.to_sym => [],
}

namespace :db do
  task :check_failures do
    frameworks = []
    Dir.glob('*/*/config.yaml') do |file|
        language, framework, _ = file.split('/')
      ENV['CONCURRENCIES'].split(',').each do |concurrency|
      ENV['ROUTES'].split(',').each do |route|
        _, uri = route.split(':')
        pp "#{language}/#{framework}/#{concurrency}_#{uri.gsub('/','_')}.json"
        row = JSON.parse(File.read("#{language}/#{framework}/#{concurrency}_#{uri.gsub('/','_')}.json"), symbolize_names: true)
        frameworks << framework if row.dig(:summary, :successRate) < 1
      end
    end
      pp frameworks.uniq
    end
  end
  task :export do
    data = { metrics: [], frameworks: [], languages: [] }
    id = 0
    Dir.glob('*/*/config.yaml') do |file|
        language, framework, _ = file.split('/')
      id += 1
      ENV['CONCURRENCIES'].split(',').each do |concurrency|
      info = {}
      MAP.each do |key, value|
        info[key] = 0
      end
      ENV['ROUTES'].split(',').each do |route|
        _, uri = route.split(':')
        data_path = "#{language}/#{framework}/#{concurrency}_#{uri.gsub('/','_')}.json"
        begin
          row = JSON.parse(File.read(data_path), symbolize_names: true)
        rescue JSON::ParserError
          next
        else
          next if row.dig(:summary, :successRate) < 1
          MAP.each do |key, value|
            if value.any?
            pp key
            info[key] += row.dig(*value)
          end   
        end
        directory = File.dirname(file)
        main_config = YAML.safe_load_file('config.yaml')
        language_config = YAML.safe_load_file(File.join(directory,'..', 'config.yaml'))
        framework_config = YAML.safe_load_file(File.join(directory,'..','..', 'config.yaml'))
        config = main_config.recursive_merge(language_config).recursive_merge(framework_config)
        scheme = 'https'
        scheme = 'http' if config['framework'].key?('unsecure')
        website = config['framework']['website']
        if website.nil?
          website = if config['framework'].key?('github')
                      "github.com/#{config['framework']['github']}"
                    elsif config['framework'].key?('gitlab')
                      "gitlab.com/#{config['framework']['gitlab']}"
                    end
        end
        unless data[:frameworks].map { |row| row[:id] }.to_a.include?(id)
          pp config
          data[:frameworks] << {
            id: id,
            version: config.dig('framework', 'version'),
            label: framework,
            language:,
            website: "#{scheme}://#{website}"
          }
        end
        unless data[:languages].map { |row| row[:label] }.to_a.include?(language)
          data[:languages] << {
            label: language,
            version: config.dig('language', 'version')
          }
        end
      end
      end
    info.each do |key, value|
      data[:metrics] << {level: concurrency, key: key, value: value/ENV['ROUTES'].split(',').count, framework_id: id}
    end
    end
    data.merge!(updated_at: Time.now.utc, version: 1)
    data.merge!(hardware: { cpus: Etc.nprocessors, memory: 7_733_008, cpu_name: 'M1 Eight-Core Processor',
                            os: Etc.uname })
    File.write('data.json', JSON.pretty_generate(data))
    File.write('data.min.json', data.to_json)
  end
end
end
