# frozen_string_literal: true

require 'pg'
require 'mustache'
require 'yaml'
require 'active_support/number_helper'

SQL = %(
    SELECT f.id, l.label AS language, f.label AS framework, c.level, sum(v.value)
        FROM frameworks AS f
            JOIN metrics AS m ON f.id = m.framework_id
            JOIN values AS v ON v.id = m.value_id
            JOIN concurrencies AS c on c.id = m.concurrency_id
            JOIN languages AS l on l.id = f.language_id
                GROUP BY 1,2,3,4;
)

namespace :db do
  task :export do
    raise 'Please provide a database' unless ENV['DATABASE_URL']

    frameworks = {}
    db = PG.connect(ENV['DATABASE_URL'])
    db.exec(SQL) do |result|
      result.each do |row|
        id, framework, language = row.values_at('id', 'framework', 'language')
        unless frameworks.key?(id)
          frameworks[id] = {
            language: language,
            framework: framework
          }
        end
        framework_config = YAML.safe_load(File.read(File.join(language, framework, 'config.yaml')))
        language_config = YAML.safe_load(File.read(File.join(language, 'config.yaml')))

        key = "concurrency_#{row['level']}"
        frameworks[id].merge!(key => row['sum'].to_f/3)
        frameworks[id].merge!(framework_config['framework'].transform_keys!(&'framework_'.method(:+)))
        frameworks[id].merge!(language_config['provider']['default'].transform_keys!(&'language_'.method(:+)))

        if framework_config['framework'].key?('name')
          frameworks[id].merge!(framework: framework_config['framework'].key?('name'))
        end
      end
    end
    db.close
    template = File.read('README.mustache.md')
    c = 0
    frameworks.values.sort { |x, y| y['concurrency_64'] <=> x['concurrency_64'] }.map do |row|
      c += 1
      row.merge!(id: c)
      row['concurrency_64'] = ActiveSupport::NumberHelper.number_to_delimited('%.2f'%row['concurrency_64'], delimiter: ' ')
      row['concurrency_256'] =  ActiveSupport::NumberHelper.number_to_delimited('%.2f'%row['concurrency_256'], delimiter: ' ')
      row['concurrency_512'] = ActiveSupport::NumberHelper.number_to_delimited('%.2f'%row['concurrency_512'], delimiter: ' ')
    end
    File.open('README.md', 'w') { |f| f.write(Mustache.render(template, { results: frameworks.values })) }
  end
end
