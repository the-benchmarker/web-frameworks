# frozen_string_literal: true

require 'dotenv'
require 'active_support'

Dir.glob('.tasks/*.rake').each { |r| load r }

MANIFESTS = {
  container: '.Dockerfile',
  build: '.Makefile'
}.freeze

Dotenv.load

class ::Hash
  def recursive_merge(h)
    merge!(h) { |_key, _old, _new| _old.instance_of?(Hash) ? _old.recursive_merge(_new) : _new }
  end
end

def default_provider
  if RbConfig::CONFIG['host_os'] =~ /linux/
    'docker'
  else
    'docker-machine'
  end
end

def commands_for(language, framework, provider = default_provider)
  config = YAML.safe_load(File.read('config.yaml'))

  directory = Dir.pwd
  main_config = YAML.safe_load(File.open(File.join(directory, 'config.yaml')))
  language_config = YAML.safe_load(File.open(File.join(directory, language, 'config.yaml')))
  framework_config = YAML.safe_load(File.open(File.join(directory, language, framework, 'config.yaml')))
  app_config = main_config.recursive_merge(language_config).recursive_merge(framework_config)
  options = { language: language, framework: framework }
  commands = { build: [], collect: [], clean: [] }

  # Compile first, only for non containers

  if app_config.key?('binaries')
    commands << "docker build -f #{MANIFESTS[:container]} -t #{language}.#{framework} ."
    commands << "docker run -td #{language}.#{framework} > cid.txt"
    app_config['binaries'].each do |out|
      if out.count(File::Separator).positive?
        FileUtils.mkdir_p(File.join(directory, File.dirname(out)))
        commands[:build] << "docker cp `cat cid.txt`:/opt/web/#{File.dirname(out)} ."
      else
        commands[:build] << "docker cp `cat cid.txt`:/opt/web/#{out} #{out}"
      end
    end
  end

  config['providers'][provider]['build'].each do |cmd|
    commands[:build] << Mustache.render(cmd, options.merge!(manifest: MANIFESTS[:container])).to_s
  end

  config['providers'][provider]['metadata'].each do |cmd|
    commands[:build] << Mustache.render(cmd, options).to_s
  end

  if app_config.key?('bootstrap') && config['providers'][provider].key?('exec')
    remote_command = config['providers'][[provider]]['exec']
    app_config['bootstrap'].each do |cmd|
      commands[:build] << Mustache.render(remote_command, options.merge!(command: cmd)).to_s
    end
  end

  if config.dig('providers', provider).key?('reboot')
    commands[:build] << config.dig('providers', provider, 'reboot')
    commands[:build] << 'sleep 30'
  end

  commands[:build] << 'curl --retry 5 --retry-delay 5 --retry-max-time 180 --retry-connrefused http://`cat ip.txt`:3000 -v'

  commands[:collect] << "LANGUAGE=#{language} FRAMEWORK=#{framework} DATABASE_URL=#{ENV['DATABASE_URL']} bundle exec rake collect"

  config.dig('providers', provider, 'clean').each do |cmd|
    commands[:clean] << Mustache.render(cmd, options).to_s
  end

  commands
end

def create_dockerfile(directory, config, template)
  config.dig('framework', 'variants').each do |variant, metadata|
    files = []
    config.dig('framework', 'files').each do |pattern|
      Dir.glob(File.join(directory, pattern)).each do |file|
        path = Pathname.new(file)
        relative_path = path.relative_path_from(Pathname.new(directory))
        variant_path = File.join(".#{variant}", relative_path)

        source = if File.exist?(File.join(directory, variant_path))
                   variant_path.to_s
                 else
                   relative_path.to_s
                 end
        files << { source: source, target: relative_path.to_s }
      end
    end

    File.open(File.join(directory, ".Dockerfile.#{variant}"), 'w') do |f|
      f.write(Mustache.render(File.read(template), {
                                files: files,
                                environment: config.dig('framework', 'environment')&.map do |k, v|
                                               { key: k, value: v }
                                             end,
                                command: metadata['command']
                              }))
    end
  end
end

task :config do
  provider = ENV.fetch('PROVIDER') { default_provider }
  collect = ENV.fetch('COLLECT', 'on')

  sieger_options = ENV.fetch('SIEGER_OPTIONS', '-r GET:/ -c 10')
  clean = ENV.fetch('CLEAN', 'on')

  main_config = YAML.safe_load(File.open(File.join(Dir.pwd, 'config.yaml')))

  Dir.glob('ruby/rails/config.yaml').each do |path|
    directory = File.dirname(path)
    language_config = YAML.safe_load(File.open(File.join(directory, '..', 'config.yaml')))

    framework_config = YAML.safe_load(File.open(File.join(directory, 'config.yaml')))

    config = main_config.recursive_merge(language_config).recursive_merge(framework_config)

    create_dockerfile(directory, config, File.join(directory, '..', 'Dockerfile'))

    language, framework = directory.split(File::SEPARATOR)

    makefile = File.open(File.join(language, framework, MANIFESTS[:build]), 'w')

    commands_for(language, framework).each do |target, commands|
      config.dig('framework', 'variants').each do |variant, _|
        makefile.write("#{target}.#{variant}:\n")
        commands.each do |command|
          makefile.write("\t #{command}\n")
        end
      end
      values = config.dig('framework', 'variants').keys.map { |variant| "#{target}.#{variant}" }.to_a
      makefile.write("#{target} : #{values.join(' ')}\n")
    end

    makefile.close
  end
end

task :clean do
  Dir.glob('**/.gitignore').each do |ignore_file|
    directory = File.dirname(ignore_file)
    next if directory.start_with?('lib')
    next if directory.start_with?('bin')

    File.foreach(ignore_file) do |line|
      line.strip!
      next if line.start_with?('!')
      next if line.start_with?('#')
      next if line.start_with?('.env')
      next if line.empty?

      Dir.glob(File.join(directory, line)).each do |file|
        if File.exist?(file)
          if File.file?(file)
            warn "Delting file #{file}"
            File.delete(file)
          elsif File.directory?(file)
            warn "Deleting directory #{file}"
            FileUtils.rm_rf(file)
          end
        end
      end
    end
  end
end
