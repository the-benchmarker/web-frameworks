require 'json'

def changed_files
  JSON.parse(ENV.fetch('FILES'))
end

def language_for(path)
  path.split(File::SEPARATOR).first
end

def dockerfile?(path)
  File.basename(path).match?(/(?:^|\.)Dockerfile(?:\.|$)/i)
end

def all_languages
  Dir.glob(File.join('*', 'config.yaml')).map { |path| language_for(path) }.sort
end

def all_frameworks(language)
  Dir.glob(File.join(language, '*', 'config.yaml')).map do |path|
    path.split(File::SEPARATOR)[1]
  end.sort
end

def selected_languages
  files = changed_files
  return all_languages if files.intersect?(%w[data.json data.min.json])

  files.filter_map do |path|
    language = language_for(path)
    next unless Dir.exist?(language)
    next if language.start_with?('.')

    language
  end.uniq.sort
end

def selected_frameworks(language)
  files = changed_files
  language_files = files.select { |path| language_for(path) == language }

  return all_frameworks(language) if files.intersect?(%w[data.json data.min.json])

  return all_frameworks(language) if language_files.any? do |path|
    dockerfile?(path) || path == "#{language}/config.yaml"
  end

  language_files.filter_map do |path|
    parts = path.split(File::SEPARATOR)
    next unless parts.length >= 3

    framework = parts[1]
    next unless File.exist?(File.join(language, framework, 'config.yaml'))

    framework
  end.uniq.sort
end

def matrix_for(language)
  selected_frameworks(language).filter_map do |framework|
    file = File.join(language, framework, 'config.yaml')
    next unless File.exist?(file)

    # Skip v/vanilla_io_uring in CI: io_uring_setup/io_uring_enter are blocked by
    # Docker's default seccomp profile on the GitHub Actions runners, so the
    # server builds but never becomes HTTP-ready. The framework code is kept in
    # the tree; remove this line once io_uring is allowed under the CI sandbox.
    # See https://github.com/the-benchmarker/web-frameworks/issues/9467
    next if language == 'v' && framework == 'vanilla_io_uring'

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
  end.uniq.take(256)
end

namespace :ci do
  task :languages do
    puts({ include: selected_languages.map { |language| { language: } } }.to_json)
  end

  task :matrix do
    language = ENV.fetch('LANGUAGE')
    puts({ include: matrix_for(language) }.to_json)
  end
end
