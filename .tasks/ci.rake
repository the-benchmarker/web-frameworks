# frozen_string_literal: true

require 'json'

namespace :ci do
  task :matrix do
    files = JSON.parse(ENV.fetch('FILES', nil))
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

      engine = config.dig('framework', 'engines')&.first

      if engine
        matrix[:include] << { language: language, framework: framework, directory: File.join(language, framework),
                              engine: engine }
      else
        warn "Configuration for #{language}/#{framework} is not correct"
      end
    end

    matrix[:include] = matrix[:include].take(256)
    puts matrix.to_json
  end
end
