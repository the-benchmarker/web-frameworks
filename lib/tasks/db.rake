# frozen_string_literal: true

require "pg"
require "mustache"
require "yaml"
require "active_support/number_helper"
require "dotenv"

Dotenv.load

SQL = %(
    SELECT f.id, l.label AS language, f.label AS framework, c.level, k.label, avg(v.value) AS value
        FROM frameworks AS f
            JOIN metrics AS m ON f.id = m.framework_id
            JOIN values AS v ON v.id = m.value_id
            JOIN concurrencies AS c on c.id = m.concurrency_id
            JOIN languages AS l on l.id = f.language_id
            JOIN keys AS k ON k.id = v.key_id
                GROUP BY 1,2,3,4,5;
)

def compute(data)
  errors = data["socket_connection_errors"].to_d + data["http_errors"].to_d + data["request_timeouts"].to_d
  duration = data["duration_ms"].to_d / 1000000
  requests = data["total_requests"].to_d

  value = (requests - errors) / duration
  begin
    value.to_i
  rescue FloatDomainError
    value = 0
  end

  value
end

namespace :db do
  task :export do
    raise "Please provide a database" unless ENV["DATABASE_URL"]

    frameworks = {}
    db = PG.connect(ENV["DATABASE_URL"])
    db.exec(SQL) do |result|
      result.each do |row|
        id, framework, language = row.values_at("id", "framework", "language")
        unless frameworks.key?(id)
          frameworks[id] = {
            language: language,
            framework: framework,
            metrics: {"concurrency_64":{},"concurrency_256":{},"concurrency_512":{}},
          }
        end
        framework_config = YAML.safe_load(File.read(File.join(language, framework, "config.yaml")))
        language_config = YAML.safe_load(File.read(File.join(language, "config.yaml")))

        key = "concurrency_#{row["level"]}".to_sym
        frameworks[id][:metrics][key].merge!(row["label"] => row["value"])
        frameworks[id].merge!(framework_config["framework"].transform_keys!(&"framework_".method(:+)))
        frameworks[id].merge!(language_config["provider"]["default"].transform_keys!(&"language_".method(:+)))

        if framework_config["framework"].key?("framework_name")
          frameworks[id].merge!(framework: framework_config["framework"]["framework_name"])
        end

        if framework_config["framework"].key?("framework_github")
          frameworks[id].merge!(framework_website: "https://github.com/#{framework_config["framework"]["framework_github"]}")
        else
          frameworks[id].merge!(framework_website: "https://#{framework_config["framework"]["framework_website"]}")
        end
      end
    end
    db.close
    template = File.read("README.mustache.md")
    results = []
    frameworks.each do |id, row|
      row.merge!(
        id: id.to_i,
        concurrency_64: compute(row[:metrics][:concurrency_64]),
        concurrency_256: compute(row[:metrics][:concurrency_256]),
        concurrency_512: compute(row[:metrics][:concurrency_512])
      )
      results << row
    end
    c=0
    results.sort! {|x,y| y[:concurrency_64].to_f <=> x[:concurrency_64].to_f}.map do |row|
      c += 1

      row.merge!(
        id: c,
        concurrency_64: ActiveSupport::NumberHelper.number_to_delimited('%.2f'% row[:concurrency_64], delimiter: ' '),
        concurrency_256: ActiveSupport::NumberHelper.number_to_delimited('%.2f'% row[:concurrency_256], delimiter: ' '),
        concurrency_512: ActiveSupport::NumberHelper.number_to_delimited('%.2f'% row[:concurrency_512], delimiter: ' ')
      )
    end
    File.open("README.md", "w") { |f| f.write(Mustache.render(template, { results: results, date: Date.today })) }
  end
end
