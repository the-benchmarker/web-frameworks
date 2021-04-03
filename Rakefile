# frozen_string_literal: true

require 'dotenv'
require 'active_support'

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

def get_config_from(directory)
  main_config = YAML.safe_load(File.open(File.join(directory, '..', '..', 'config.yaml')))

  language_config = YAML.safe_load(File.open(File.join(directory, '..', 'config.yaml')))

  framework_config = YAML.safe_load(File.open(File.join(directory, 'config.yaml')))

  config = main_config.recursive_merge(language_config).recursive_merge(framework_config)

  keys = []
  keys << language_config['default'].keys if language_config['default']
  keys << framework_config['framework'].keys if framework_config['framework']

  keys.flatten!.uniq.each do |key|
    default = language_config.dig('default', key)

    next unless default

    base = framework_config.dig('framework', key)
    if base
      case base
      when Array
        default.push(*base)
      when Hash
        default.merge!(base)
      end
    end

    framework_config['framework'][key] = default
  end

  config
end

def default_provider
  if RbConfig::CONFIG['host_os'] =~ /linux/
    'docker'
  else
    'docker-machine'
  end
end

def commands_for(language, framework, variant, provider = default_provider)
  config = YAML.safe_load(File.read('config.yaml'))

  directory = Dir.pwd
  main_config = YAML.safe_load(File.open(File.join(directory, 'config.yaml')))
  language_config = YAML.safe_load(File.open(File.join(directory, language, 'config.yaml')))
  framework_config = YAML.safe_load(File.open(File.join(directory, language, framework, 'config.yaml')))
  app_config = main_config.recursive_merge(language_config).recursive_merge(framework_config)
  options = { language: language, framework: framework, variant: variant,
              manifest: "#{MANIFESTS[:container]}.#{variant}" }
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
    commands[:build] << Mustache.render(cmd, options).to_s
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

  commands[:build] << "curl --retry 5 --retry-delay 5 --retry-max-time 180 --retry-connrefused http://`cat #{language}/#{framework}/ip-#{variant}.txt`:3000 -v"

  commands[:collect] << "HOSTNAME=`cat #{language}/#{framework}/ip-#{variant}.txt` VARIANT=#{variant} LANGUAGE=#{language} FRAMEWORK=#{framework} DATABASE_URL=#{ENV['DATABASE_URL']} bundle exec rake collect"

  config.dig('providers', provider, 'clean').each do |cmd|
    commands[:clean] << Mustache.render(cmd, options).to_s
  end

  commands
end

def create_dockerfile(directory, config, template)
  config.dig('framework', 'engines').each do |variant, metadata|
    files = []
    paths = config.dig('framework', 'files')
    paths.push(*metadata['files']) if metadata['files']

    paths.each do |pattern|
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
      f.write(Mustache.render(File.read(template),
                              config['framework']
                                .merge(metadata)
                                .merge('files' => files)
                                .merge('environment' => config.dig('framework', 'environment')&.map do |k, v|
                                                          "#{k}=#{v}"
                                                        end)))
    end
  end
end

task :config do
  # Dir.glob(['php/chubbyphp/config.yaml', 'php/laravel/config.yaml', 'ruby/*/config.yaml']).each do |path|
  Dir.glob('*/*/config.yaml').each do |path|
    directory = File.dirname(path)
    config = get_config_from(directory)
    next unless config.dig('framework', 'engines')

    create_dockerfile(directory, config, File.join(directory, '..', 'Dockerfile'))

    language, framework = directory.split(File::SEPARATOR)

    makefile = File.open(File.join(language, framework, MANIFESTS[:build]), 'w')

    config.dig('framework', 'engines').each do |variant, _|
      commands_for(language, framework, variant).each do |target, commands|
        makefile.write("#{target}.#{variant}:\n")
        commands.each do |command|
          makefile.write("\t #{command}\n")
        end
      end
    end

    command = config.dig('framework', 'engines').map { |v, _| ["build.#{v}", "collect.#{v}", "clean.#{v}"] }.join(' ')

    makefile.write("run-all : #{command}\n")

    makefile.close
  end
end

task :clean do
  Dir.glob('*/*/.gitignore').each do |ignore_file|
    directory = File.dirname(ignore_file)

    File.foreach(ignore_file) do |line|
      line.strip!
      next if line.start_with?('!')
      next if line.start_with?('#')
      next if line.start_with?('.env')
      next if line.empty?

      Dir.glob(File.join(directory, line)).each do |path|
        if File.exist?(path)
          if File.file?(path)
            warn "Delting file #{path}"
            File.delete(path)
          elsif File.directory?(path)
            warn "Deleting directory #{path}"
            FileUtils.rm_rf(path)
          end
        end
      end
    end
  end
end

Dir.glob('.tasks/*.rake').each { |r| load r }
