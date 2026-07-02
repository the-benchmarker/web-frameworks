require 'json'

def dockerfile_or_language_config?(path)
  path.end_with?('Dockerfile') || (path.split(File::SEPARATOR).size == 2 && path.end_with?('config.yaml'))
end

def input_files
  files = JSON.parse(ENV.fetch('FILES'))

  return Dir.glob(File.join('*', '*', 'config.yaml')) if files.include?('data.json')

  languages = files
              .select { |path| dockerfile_or_language_config?(path) }
              .map { |path| path.split(File::SEPARATOR).first }
              .uniq

  files + languages.flat_map { |lang| Dir.glob(File.join(lang, '*', 'config.yaml')) }
end

namespace :ci do
  desc 'Output list of affected languages (level 0)'
  task :languages do
    languages = input_files.filter_map do |file|
      next if file.start_with?('.')
      next if file.count(File::SEPARATOR) < 1
      next unless File.exist?(file)

      file.split(File::SEPARATOR).first
    end

    languages = languages.uniq.select { |lang| Dir.exist?(lang) && !lang.start_with?('.') }

    puts({ language: languages }.to_json)
  end

  desc 'Output list of frameworks for a given language (level 1)'
  task :frameworks do
    language = ENV.fetch('LANGUAGE')
    frameworks = Dir.glob(File.join(language, '*', 'config.yaml')).filter_map do |path|
      framework = path.split(File::SEPARATOR)[1]

      # Skip v/vanilla_io_uring in CI: io_uring_setup/io_uring_enter are blocked by
      # Docker's default seccomp profile on the GitHub Actions runners, so the
      # server builds but never becomes HTTP-ready.
      # See https://github.com/the-benchmarker/web-frameworks/issues/9467
      next if language == 'v' && framework == 'vanilla_io_uring'

      config = get_config_from(File.join(Dir.pwd, language, framework))
      engines = config.dig('framework', 'engines')

      next unless engines&.any?

      framework
    end

    puts({ framework: frameworks.uniq }.to_json)
  end

  desc 'Output list of engines for a given language/framework (level 2)'
  task :engines do
    language = ENV.fetch('LANGUAGE')
    framework = ENV.fetch('FRAMEWORK')

    config = get_config_from(File.join(Dir.pwd, language, framework))
    engines = config.dig('framework', 'engines') || []

    puts({ engine: engines }.to_json)
  end
end
