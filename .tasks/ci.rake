# frozen_string_literal: true

require "json"

namespace :ci do
  task :matrix do
    files = JSON.parse(ENV["FILES"])
    matrix = { include: [] }
    
    files = Dir.glob('*/*/config.yaml') if files.include?('data.json')
    
    files.each do |file|
      next if file.start_with?(".")
      language, framework, _ = file.split(File::SEPARATOR)
      matrix[:include] << { language: language, framework: framework, directory: File.join(language, framework) }
    end
    
    warn matrix.to_json
    puts matrix.to_json
  end
end
