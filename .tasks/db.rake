require 'pg'
require 'mustache'
require 'yaml'
require 'active_support/number_helper'
require 'dotenv'
require 'etc'

Dotenv.load

SQL = %(
    SELECT f.id, l.label AS language, f.label AS framework, c.level, k.label, avg(v.value) AS value
        FROM frameworks AS f
            JOIN metrics AS m ON f.id = m.framework_id
            JOIN values AS v ON v.id = m.value_id
            JOIN concurrencies AS c on c.id = m.concurrency_id
            JOIN languages AS l on l.id = f.language_id
            JOIN keys AS k ON k.id = v.key_id
                GROUP BY 1,2,3,4,5
).freeze

def compute(data)
  errors = data['http_errors'].to_d
  duration = data['duration_ms'].to_d / 1_000_000
  requests = data['total_requests'].to_d

  (requests - errors) / duration
end

namespace :db do
  task :check_failures do
    results = JSON.parse(File.read('data.json'))
    results['frameworks'].map { _1['label'] }
    failing_frameworks = results['metrics'].filter_map do |row|
      row['framework_id'] if row['label'] == 'total_requests_per_s' && (row['value']).zero?
    end
    list_of = Dir.glob('*/*/config.yaml').map { _1.split('/')[1] }
    $stdout.puts "Failing : #{results['frameworks'].filter_map do |row|
      row['label'] if failing_frameworks.include?(row['id'])
    end}"
    $stdout.puts "Missing : #{list_of - results['frameworks'].map { _1['label'] }}"
  end
  task :raw_export do
    raise 'Please provide a database' unless ENV['DATABASE_URL']

    data = { metrics: [], frameworks: [], languages: [] }
    db = PG.connect(ENV.fetch('DATABASE_URL', nil))
    db.exec("select row_to_json(t) from (#{SQL}) as t") do |result|
      result.each do |row|
        info = JSON.parse(row['row_to_json'], symbolize_names: true)
        framework_id = info.delete :id
        info[:framework_id] = framework_id
        language = info.delete :language
        framework = info.delete :framework
        main_config = YAML.safe_load_file(File.join('config.yaml'))
        language_config = YAML.safe_load_file(File.join(language, 'config.yaml'))
        framework_config = YAML.safe_load_file(File.join(language, framework, 'config.yaml'))
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
        unless data[:frameworks].map { |row| row[:id] }.to_a.include?(framework_id)
          data[:frameworks] << {
            id: framework_id,
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
        data[:metrics] << info
        next
      end
    end
    data.merge!(updated_at: Time.now.utc, version: 1)
    data.merge!(hardware: { cpus: Etc.nprocessors, memory: 16_282_676, cpu_name: 'AMD FX-8320E Eight-Core Processor',
                            os: Etc.uname })
    File.write('data.json', JSON.pretty_generate(data))
    File.write('data.min.json', data.to_json)
  end
end
