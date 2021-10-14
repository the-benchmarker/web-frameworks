# frozen_string_literal: true

require 'json'

namespace :ci do
  task :matrix do
    files = JSON.parse(ENV['FILES'])
    matrix = { include: [] }

    files = Dir.glob('*/*/config.yaml') if files.include?('data.json')

    files += files
             .find_all { |path| path.end_with?('Dockerfile') }
             .map { |path| path.split(File::SEPARATOR).shift }
             .flat_map { |language| Dir.glob(File.join(language, '*', 'config.yaml')) }

    files.each do |file|
      next if file.start_with?('.')

      next if file.count(File::SEPARATOR) < 2

      language, framework, = file.split(File::SEPARATOR)

      config = get_config_from(File.join(Dir.pwd, language, framework))

      config.dig('framework', 'engines')&.each do |engine|
        next if matrix[:include].detect { |row| row[:framework] == framework && row[:engine] == engine }

        matrix[:include] << { language: language, framework: framework, directory: File.join(language, framework), engine: engine }
      end
    end
    warn matrix.to_json
    puts matrix.to_json
  end
end
