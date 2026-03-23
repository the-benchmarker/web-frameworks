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
  task :matrix do
    matrix = input_files.filter_map do |file|
      next if file.start_with?('.')
      next if file.count(File::SEPARATOR) < 2
      next unless File.exist?(file)

      language, framework, = file.split(File::SEPARATOR)

      config = get_config_from(File.join(Dir.pwd, language, framework))
      engine = config.dig('framework', 'engines')&.first

      unless engine
        warn "Configuration for #{language}/#{framework} is not correct"
        next
      end

      {
        language:,
        framework:,
        directory: File.join(language, framework),
        engine:
      }
    end

    matrix = matrix.uniq.take(256)

    puts({ include: matrix }.to_json)
  end
end
