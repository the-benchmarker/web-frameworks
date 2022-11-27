# frozen_string_literal: true

require 'json'

namespace :ci do
  task :languages do
    added_files = JSON.parse(File.read(File.expand_path(ENV['INPUT1'])))
    modified_files = JSON.parse(File.read(File.expand_path(ENV['INPUT2'])))
    files = (added_files + modified_files)
    matrix = { include: [] }

    files = Dir.glob('*/*/config.yaml') if files.include?('data.json')

    files += files
             .find_all { |path| path.end_with?('Dockerfile') }
             .map { |path| path.split(File::SEPARATOR).shift }
             .flat_map { |language| Dir.glob(File.join(language, '*', 'config.yaml')) }

    files.each do |file|
      next if file.start_with?('.')

      next if file.count(File::SEPARATOR) < 2

      language, _ = file.split(File::SEPARATOR)
      next if matrix[:include].detect do |row|
        row[:language] == language
      end

      matrix[:include] << { language: language }
    end
    
    puts "json=#{matrix.to_json}"
  end
  task :frameworks do
    added_files = JSON.parse(File.read(File.expand_path(ENV['INPUT1'])))
    modified_files = JSON.parse(File.read(File.expand_path(ENV['INPUT2'])))
    files = (added_files + modified_files)
    matrix = { include: [] }

    files = Dir.glob('*/*/config.yaml') if files.include?('data.json')

    files += files
             .find_all { |path| path.end_with?('Dockerfile') }
             .map { |path| path.split(File::SEPARATOR).shift }
             .flat_map { |language| Dir.glob(File.join(language, '*', 'config.yaml')) }

    files.each do |file|
      next if file.start_with?('.')

      next if file.count(File::SEPARATOR) < 2

      language, framework, _ = file.split(File::SEPARATOR)
      next if matrix[:include].detect do |row|
        row[:framework] == framework
      end

      matrix[:include] << { framework: framework }
    end
    
    puts "json=#{matrix.to_json}"
  end
end
