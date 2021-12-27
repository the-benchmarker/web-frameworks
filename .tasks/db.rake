# frozen_string_literal: true

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
)

def compute(data)
  errors = data['http_errors'].to_d
  duration = data['duration_ms'].to_d / 1_000_000
  requests = data['total_requests'].to_d

  (requests - errors) / duration
end

namespace :db do
  task :raw_export do
    raise 'Please provide a database' unless ENV['DATABASE_URL']

    data = {metrics: [], frameworks: [], languages: [] }
    db = PG.connect(ENV['DATABASE_URL'])
    db.exec("select row_to_json(t) from (#{SQL}) as t") do |result|
      result.each do |row|
        info = JSON.parse(row['row_to_json'], symbolize_names: true)
        framework_id = info.delete :id
        info[:framework_id] = framework_id
        language = info.delete :language
        framework = info.delete :framework
        framework_config = YAML.safe_load(File.read(File.join(language, framework, 'config.yaml')))
        language_config = YAML.safe_load(File.read(File.join(language, 'config.yaml')))
        scheme = 'https'
        scheme = 'http' if framework_config['framework'].key?('unsecure')
        website = if framework_config['framework'].key?('github')
                    "github.com/#{framework_config['framework']['github']}"
                  elsif framework_config['framework'].key?('gitlab')
                    "gitlab.com/#{framework_config['framework']['gitlab']}"
                  else
                    (framework_config['framework']['website']).to_s
                  end
        unless data[:frameworks].map{|row|row[:id]}.to_a.include?(framework_id)
          data[:frameworks] << {
            id: framework_id,
            version: framework_config.dig('framework', 'version'),
            label: framework,
            language: language,
            website:  scheme + "://" + website
          }
        end
        unless data[:languages].map{|row|row[:label]}.to_a.include?(language)
          data[:languages] << {
            label: language,
            version: language_config.dig('provider', 'default', 'language')
          }
        end
        data[:metrics] << info
        next
      end
    end
    data.merge!(updated_at: Time.now.utc)
    data.merge!(hardware: {cpus: Etc.nprocessors, memory: 16282676, cpu_name: 'AMD FX-8320E Eight-Core Processor', os: Etc.uname})
    File.open('data.json','w').write(JSON.pretty_generate(data))
    File.open('data.min.json','w').write(data.to_json)
  end
end
