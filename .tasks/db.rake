require 'pg'
require 'yaml'
require 'active_support/number_helper'
require 'etc'

SQL = <<~SQL.freeze
  SELECT
    f.id,
    l.label AS language,
    f.label AS framework,
    c.level,
    k.label,
    avg(v.value) AS value
  FROM frameworks AS f
  JOIN metrics AS m ON f.id = m.framework_id
  JOIN values AS v ON v.id = m.value_id
  JOIN concurrencies AS c on c.id = m.concurrency_id
  JOIN languages AS l on l.id = f.language_id
  JOIN keys AS k ON k.id = v.key_id
  GROUP BY 1,2,3,4,5
SQL

def compute(data)
  errors = data['http_errors'].to_d
  duration = data['duration_ms'].to_d / 1_000_000
  requests = data['total_requests'].to_d

  (requests - errors) / duration
end

namespace :db do
  task :md_export do
    results = JSON.load_file('data.json', symbolize_names: true)

    frameworks = results[:frameworks].to_h { [it[:id], it[:label]] }

    reqs = results[:metrics].filter_map do |m|
      next unless m[:level] == 256 && m[:label] == 'total_requests_per_s'

      { framework: frameworks[m[:framework_id]], level: m[:level], value: m[:value] }
    end

    pp reqs.sort_by { it[:value] }
  end

  task :check_failures do
    results = JSON.load_file('data.json', symbolize_names: true)

    failing_frameworks_ids = results[:metrics].filter_map do |m|
      m[:framework_id] if m[:label] == 'total_requests_per_s' && m[:value].zero?
    end

    failing_frameworks = results[:frameworks].filter_map do |row|
      row[:label] if failing_frameworks_ids.include?(row[:id])
    end

    existing_frameworks = Dir.glob('*/*/config.yaml').map { |path| path.split('/')[1] }

    all_framework_labels = results[:frameworks].map { it[:label] }

    puts "Failing : #{failing_frameworks}"
    puts "Missing : #{existing_frameworks - all_framework_labels}"
  end

  task :raw_export do
    database_url = ENV.fetch('DATABASE_URL')
    db = PG.connect(database_url)

    data = { metrics: [], frameworks: [], languages: [] }
    main_config = YAML.load_file('config.yaml')

    db.exec("SELECT row_to_json(t) FROM (#{SQL}) AS t") do |result|
      result.each do |row|
        info = JSON.parse(row['row_to_json'], symbolize_names: true)
        framework_id = info.delete :id
        info[:framework_id] = framework_id
        language = info.delete :language
        framework = info.delete :framework
        language_config = YAML.safe_load_file(File.join(language, 'config.yaml'))
        framework_config = YAML.safe_load_file(File.join(language, framework, 'config.yaml'))

        config = main_config.recursive_merge(language_config).recursive_merge(framework_config)
        scheme = config['framework'].key?('unsecure') ? 'http' : 'https'
        website = if config['framework']['website']
                    config['framework']['website']
                  elsif config['framework'].key?('github')
                    "github.com/#{config['framework']['github']}"
                  elsif config['framework'].key?('gitlab')
                    "gitlab.com/#{config['framework']['gitlab']}"
                  end

        unless data[:frameworks].map { it[:id] }.to_a.include?(framework_id)
          data[:frameworks] << {
            id: framework_id,
            version: config.dig('framework', 'version'),
            label: framework,
            language:,
            website: "#{scheme}://#{website}"
          }
        end

        unless data[:languages].map { it[:label] }.to_a.include?(language)
          data[:languages] << {
            label: language,
            version: config.dig('language', 'version')
          }
        end

        data[:metrics] << info
      end
    end

    data.merge!(
      updated_at: Time.now.utc,
      version: 1,
      hardware: {
        cpus: Etc.nprocessors,
        memory: 7_733_008,
        cpu_name: 'M1 Eight-Core Processor',
        os: Etc.uname
      }
    )

    File.write('data.json', JSON.pretty_generate(data))
    File.write('data.min.json', data.to_json)
  end
end
