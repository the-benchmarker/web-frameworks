# frozen_string_literal: true

require 'git'

namespace :ci do
  task :matrix do
    base = ENV['BASE_COMMIT']
    last = ENV['LAST_COMMIT']
    workdir = ENV.fetch('GITHUB_WORKSPACE') { Dir.pwd }
    frameworks = []
    files = []
    git = Git.open(Dir.pwd)

    diff = git.gtree(last).diff(base).each { |diff| files << diff.path }

    files.each do |file|
      next unless file.end_with?('config.yaml')

      parts = file.split(File::SEPARATOR)

      case parts.size
      when 2 # We are modifying a language
        Dir.glob("#{parts.first}/*/config.yaml").each do |path|
          subs = path.split(File::SEPARATOR)
          frameworks << subs[0..1].join(File::SEPARATOR)
        end
      when 3 # We are modifying a framework
        frameworks << parts[0..1].join(File::SEPARATOR)
      end
    end

    if base.empty? || last.empty?
      Dir.glob('*/*/config.yaml').each do |path|
        parts = path.split(File::SEPARATOR)
        frameworks << parts[0..1].join(File::SEPARATOR)
      end
    end

    matrix = { include: [] }
    frameworks.uniq.each do |framework|
      matrix[:include] << { directory: framework, framework: framework }
    end

    puts matrix.to_json
  end
end
