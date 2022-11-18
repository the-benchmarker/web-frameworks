# frozen_string_literal: true

require 'json'

namespace :ci do
  task :languages do
    added_files = JSON.parse(File.read(File.expand_path(ENV['INPUT1'])))
    modified_files = JSON.parse(File.read(File.expand_path(ENV['INPUT1'])))
    files = (added_files + modified_files)
    warn files
    matrix = { include: [] }

    files = Dir.glob('*/*/config.yaml') if files.include?('data.json')

    files += files
             .find_all { |path| path.end_with?('Dockerfile') }
             .map { |path| path.split(File::SEPARATOR).shift }
             .flat_map { |language| Dir.glob(File.join(language, '*', 'config.yaml')) }

    files.take(256).each do |file|
      next if file.start_with?('.')

      next if file.count(File::SEPARATOR) < 2

      language, framework, = file.split(File::SEPARATOR)

      next if matrix[:include].detect do |row|
                row[:framework] == framework
              end

      matrix[:include] << { language: language, framework: framework,
                            directory: File.join(language, framework) }
    end
    warn matrix
  end
end
