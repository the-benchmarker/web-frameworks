require 'active_support'
require 'yaml'
require 'mustache'
require 'shellwords'
require 'json'

MANIFESTS = {
  container: '.Dockerfile',
  build: '.Makefile'
}.freeze

CUSTOM_CONFIG_KEYS = %w[version engines website github].freeze

def architecture
  RUBY_PLATFORM.start_with?('aarch64') ? 'arm64' : 'amd64'
end

def arch
  RUBY_PLATFORM.start_with?('aarch64') ? 'aarch64' : 'x86_64'
end

def get_config_from(directory, engines_as_list: true)
  main_config = YAML.safe_load_file(File.join(directory, '..', '..', 'config.yaml'))
  language_config = YAML.safe_load_file(File.join(directory, '..', 'config.yaml'))
  framework_config = YAML.safe_load_file(File.join(directory, 'config.yaml'))

  config = main_config.recursive_merge(language_config).recursive_merge(framework_config)

  engines = config.dig('framework', 'engines')

  if engines && !engines_as_list
    config['framework']['engines'] = engines.map do |row|
      if row.is_a?(String) && config.dig('language', 'engines', row)
        { row => config.dig('language', 'engines', row) }
      else
        row
      end
    end
  end

  skippable_keys = framework_config['framework'].select { |_, v| v.nil? }.keys

  config['framework'].except!(*skippable_keys)
  config['language'].except!(*skippable_keys)

  config
end

def override_or_merge(*values)
  value = values.compact.last

  values.reverse_each do |v|
    next unless v && value

    case v
    when Array
      value = (v + value).uniq
    when String
      value = v
    end
  end

  value
end

def custom_config(dict1, dict2, dict3)
  keys = (dict1.keys + dict2.keys + dict3.keys).uniq

  keys.each_with_object({}) do |key, data|
    next if CUSTOM_CONFIG_KEYS.include?(key)

    data[key] = override_or_merge(dict3[key], dict2[key], dict1[key])
  end
end

def commands_for(language, framework, variant, provider = 'docker')
  concurrencies = ENV.fetch('CONCURRENCIES', '10')
  routes = ENV.fetch('ROUTES', 'GET:/')

  directory = Dir.pwd

  main_config = YAML.safe_load_file(File.join('config.yaml'))
  language_config = YAML.safe_load_file(File.join(directory, language, 'config.yaml'))
  framework_config = YAML.safe_load_file(File.join(directory, language, framework, 'config.yaml'))
  app_config = main_config.recursive_merge(language_config).recursive_merge(framework_config)

  options = { language: language, framework: framework, variant: variant, manifest: "#{MANIFESTS[:container]}.#{variant}" }
  commands = { build: [], collect: [], clean: [], warmup: [], unbuild: [], test: [], 'memory-idle': [] }
  prerequisites = Hash.new { |h, k| h[k] = [] }

  # Compile first, only for non containers
  if app_config.key?('binaries') && !provider.start_with?('docker', 'podman')
    commands << "docker build -f #{MANIFESTS[:container]}.#{variant} -t #{language}.#{framework} ."
    commands << "docker run -td #{language}.#{framework} > cid.txt"
    app_config['binaries'].each do |out|
      if out.count(File::Separator).positive?
        FileUtils.mkdir_p(File.join(directory, File.dirname(out)))
        commands[:build] << "docker cp `cat cid-#{variants}.txt`:/opt/web/#{File.dirname(out)} ."
      else
        commands[:build] << "docker cp `cat cid-#{variants}.txt`:/opt/web/#{out} #{out}"
      end
    end
  end

  main_config['providers'][provider]['build'].each do |cmd|
    commands[:build] << Mustache.render(cmd, options).to_s
  end

  main_config['providers'][provider]['metadata'].each do |cmd|
    commands[:build] << Mustache.render(cmd, options).to_s
  end

  if app_config.key?('bootstrap') && main_config['providers'][provider].key?('exec')
    remote_command = main_config['providers'][provider]['exec']
    app_config['bootstrap'].each do |cmd|
      commands[:build] << Mustache.render(remote_command, options.merge!(command: cmd)).to_s
    end
  end

  if main_config.dig('providers', provider).key?('reboot')
    commands[:build] << main_config.dig('providers', provider, 'reboot')
  end

  # threads = ENV.fetch('THREADS') { Etc.nprocessors } # unused
  # duration = ENV.fetch('DURATION', 10) # unused

  hostname = File.join(directory, language, framework, "ip-#{variant}.txt")
  File.join(directory, language, framework, "cid-#{variant}.txt")
  File.join(File.dirname(__FILE__), 'memory_sampler.rb')
  oha_path = command_available?('oha') ? 'oha' : File.expand_path('~/.cargo/bin/oha')

  commands[:warmup] << "#{oha_path} --wait-ongoing-requests-after-deadline --no-tui --disable-keepalive --latency-correction -z 5s http://`cat #{hostname}`:3000/"
  commands[:test] << "ENGINE=#{variant} LANGUAGE=#{language} FRAMEWORK=#{framework} bundle exec rspec .spec"

  concurrencies.split(',').each do |concurrency|
    target = :"collect-#{concurrency}"
    commands[target] = [] unless commands.key?(target)

    File.join(directory, language, framework, '.results', concurrency, 'memory.json')
    oha_cmds = []

    routes.split(',').each do |route|
      method, uri = route.split(':')
      output = File.join(directory, language, framework, '.results', concurrency, "#{uri.tr('/', '_')}.json")
      oha_cmds << "#{oha_path} --wait-ongoing-requests-after-deadline --no-tui --disable-keepalive --latency-correction -c #{concurrency} -z 15s -m #{method} --output-format json --output #{output} http://`cat #{hostname}`:3000#{uri}"
    end

    # Start memory sampler in background, run all oha calls, then stop sampler
    # commands[target] << "ruby #{sampler} --cid #{cid_file} --out #{memory_out} & SAMPLER_PID=$$!; #{oha_cmds.join('; ')}; kill $$SAMPLER_PID"
    commands[target] << oha_cmds.join('; ')
  end

  concurrencies.split(',').each do |c|
    prerequisites[:collect] << "collect-#{c}"
  end

  main_config['providers'][provider]['unbuild'].each do |cmd|
    commands[:unbuild] << Mustache.render(cmd, options).to_s
  end

  main_config['providers'][provider]['clean'].each do |cmd|
    commands[:clean] << Mustache.render(cmd, options).to_s
  end

  { commands: commands, prerequisites: prerequisites }
end

def create_dockerfile(directory, engine, config)
  path = File.join(Dir.pwd, directory, '..', "#{engine}.Dockerfile")
  path = File.readlink(path) if File.symlink?(path)
  path = File.join(Dir.pwd, directory, '..', 'Dockerfile') unless File.exist?(path)

  # Path to remove stability suffix (stable, beta, alpha, or version) of php extensions

  files = []

  Dir.glob(config['files']).each do |file|
    variant_file = file.gsub(directory, File.join(directory, ".#{engine}"))

    target = if file.include?(".#{engine}")
               file.gsub(".#{engine}/", '').gsub("#{directory}/", '')
             else
               file.gsub("#{directory}/", '')
             end

    source = File.exist?(variant_file) ? variant_file : file

    files << { source: source.gsub("#{directory}/", ''), target: target }
  end

  static_files = []

  if config['static_files']
    Dir.glob(config['static_files']).each do |static_file|
      static_files << { source: static_file.gsub("#{directory}/", ''), target: static_file.gsub("#{directory}/", '') }
    end
  end

  compiler = config.dig('language', 'compiler')
  config['language']['compiler'] = { compiler => true } if compiler

  config['command'] = shell_to_json_array(config['command']) if config['command']
  config['options'] = shell_to_json_array(config['options']) if config['options']

  template = File.read(path)
  config
    .merge!(template_variables)
    .merge!({ if: template_conditions })
    .merge!(files:, static_files:, environment: config['environment']&.map do |k, v|
                                                  "#{k}=#{v}"
                                                end)

  File.write(File.join(directory, ".Dockerfile.#{engine}"), Mustache.render(template, config))
end

# This method returns a hash with variables usable in dockerfiles
def template_variables
  { arch:, architecture: }
end

def template_conditions
  template_variables.flat_map { |k, v| { k.to_s => { v => true } } }.reduce(:merge)
end

def entrypoint_args_json(array)
  json_elements = array.map(&:to_json).join(', ')
  "[#{json_elements}]"
end

def shell_to_json_array(value)
  case value
  when Array
    entrypoint_args_json(value)
  when String
    normalized = normalize_shell(value)

    if normalized.start_with?('sh -c ')
      parts = normalized.split(' ', 3)
      return entrypoint_args_json(parts)
    end

    # checking for shell commands, variables, or obtaining variables in `command` Guile
    if normalized.match?(/\$\(|\$\{/) || normalized.match?(/\(\$\w+\)/)
      parts = normalized.scan(/\A(?:[A-Z_]+=.*?\s+)+/).first
      return entrypoint_args_json(['sh', '-c', "exec #{normalized}"]) unless parts

      rest = normalized[parts.length..].strip
      return entrypoint_args_json(['sh', '-c', "#{parts}exec #{rest}"])
    end

    entrypoint_args_json(Shellwords.split(normalized))
  else
    raise "Invalid command: #{value}"
  end
end

def generate_dockerfiles(directory, engines, config)
  language_config = config['language']
  framework_config = config['framework']

  engines.each do |engine|
    engine.each do |name, data|
      variables = custom_config(language_config, framework_config, data)
      variables['files'].each { |f| f.prepend(directory, File::SEPARATOR) unless f.start_with?(directory) }.uniq!
      variables['static_files']&.each do |f|
        f.prepend(directory, File::SEPARATOR) unless f.start_with?(directory)
      end&.uniq!

      create_dockerfile(directory, name, config.merge(variables))
    end
  end
end

def create_makefile(language, framework, engines)
  path = File.join(language, framework, MANIFESTS[:build])

  File.open(path, 'w') do |makefile|
    engine = engines.first.keys.first

    result = commands_for(language, framework, engine)
    commands = result[:commands]
    prerequisites = result[:prerequisites]

    commands.each do |target, cmds|
      prereqs = prerequisites[target]
      makefile.puts "#{target}: #{prereqs.join(' ')}".rstrip
      cmds.each { |cmd| makefile.puts("\t#{cmd}") }
    end

    names = engines.flat_map(&:keys)
    command = names.flat_map { |n| ["build.#{n}", "collect.#{n}", "clean.#{n}"] }.join(' ')

    makefile.puts "run-all: #{command}"
  end
end

desc 'Create Dockerfiles and Makefiles'
task :config do
  Dir.glob('*/*/config.yaml').each do |path|
    dir = File.dirname(path)

    config = get_config_from(dir, engines_as_list: false)
    engines = config.dig('framework', 'engines')

    generate_dockerfiles(dir, engines, config)

    language, framework = dir.split(File::SEPARATOR)
    create_makefile(language, framework, engines)
  end
end

desc 'Get framework by success rate'
task :by_success do
  frameworks = Hash.new { |h, k| h[k] = Set.new }

  Dir.glob('*/**/.results/**/*.json').each do |file|
    data = JSON.load_file(file, symbolize_names: true)
    rate = data.dig(:summary, :successRate)&.round(2)
    next unless rate && rate < 1

    name = file.split('/').first
    frameworks[rate] << name
  end

  pp frameworks.map { |success_rate, framework| [success_rate, framework.join(',')] }
end

desc 'Clean unused file'
task :clean do
  Dir.glob('*/**/.gitignore').each do |ignore_file|
    dir = File.dirname(ignore_file)

    File.foreach(ignore_file) do |line|
      line.strip!
      next if line.empty? || line.start_with?('!', '#', '.env')

      Dir.glob(File.join(dir, line)).each do |path|
        next unless File.exist?(path)

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
