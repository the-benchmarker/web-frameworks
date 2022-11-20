# frozen_string_literal: true

require 'json'

namespace :ci do
  task :languages do
    added_files = JSON.parse(File.read(File.expand_path(ENV['INPUT1'])))
    modified_files = JSON.parse(File.read(File.expand_path(ENV['INPUT2'])))
    files = (added_files + modified_files)
    languages = []

    files = Dir.glob('*/*/config.yaml') if files.include?('data.json')

    files += files
             .find_all { |path| path.end_with?('Dockerfile') }
             .map { |path| path.split(File::SEPARATOR).shift }
             .flat_map { |language| Dir.glob(File.join(language, '*', 'config.yaml')) }

    files.each do |file|
      next if file.start_with?('.')

      next if file.count(File::SEPARATOR) < 2

      language, = file.split(File::SEPARATOR)

      languages << language
    end
    warn "Writting updated languages in #{File.expand_path(ENV['OUTPUT'])}"
    File.write(File.expand_path(ENV['OUTPUT']), languages.to_json)
  end
end
