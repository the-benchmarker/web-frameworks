require 'active_support'
require 'yaml'
require 'mustache'
require 'shellwords'
require 'json'
require 'etc'
require_relative 'workloads'

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
      value = (value + v).uniq
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
  directory = Dir.pwd

  main_config = YAML.safe_load_file('config.yaml')
  language_config = YAML.safe_load_file(File.join(directory, language, 'config.yaml'))
  framework_config = YAML.safe_load_file(File.join(directory, language, framework, 'config.yaml'))
  app_config = main_config.recursive_merge(language_config).recursive_merge(framework_config)
  specs = app_config.fetch('specs', 'v1')
  routes = BenchmarkWorkloads.routes(specs: specs)

  # Pin the server and load generator independently when CPU sets are supplied.
  server_cpus = ENV.fetch('SERVER_CPUS', nil)
  load_cpus = ENV.fetch('LOAD_CPUS', nil)
  cpuset = server_cpus ? "--cpuset-cpus=#{server_cpus} " : ''
  taskset = load_generator_prefix(load_cpus)

  options = { language: language, framework: framework, variant: variant, cpuset: cpuset, manifest: "#{MANIFESTS[:container]}.#{variant}" }
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

  # Closed-loop runs send each connection's next request after its response.
  duration = ENV.fetch('DURATION', '15s')

  # An optional latency pass uses a fixed rate or a percentage of measured throughput.
  latency_rate = ENV.fetch('LATENCY_RATE', '').strip

  # Use the assigned CPUs unless the thread count is explicitly overridden.
  threads = ENV.fetch('THREADS') { cpuset_size(load_cpus) || Etc.nprocessors }

  hostname = File.join(directory, language, framework, "ip-#{variant}.txt")
  cid_file = File.join(directory, language, framework, "cid-#{variant}.txt")
  saturation_probe = File.join(File.dirname(__FILE__), 'saturation.rb')
  latency_probe = File.join(File.dirname(__FILE__), 'latency_rate.rb')
  zrk = "#{taskset}zrk --plain --closed -t #{threads}"

  # Warm up with the same request scripts and closed-loop load as collection.
  routes.each do |method, uri|
    request = BenchmarkWorkloads.request(method, uri, root: directory)
    args = ['--connections', '50', '--duration', '5s', *request[:options]].shelljoin
    url = "http://`cat #{Shellwords.escape(hostname)}`:3000#{Shellwords.escape(uri)}"
    commands[:warmup] << "#{zrk} #{args} #{url} #{request[:arguments].shelljoin}".rstrip
  end
  commands[:test] << "ENGINE=#{variant} LANGUAGE=#{language} FRAMEWORK=#{framework} bundle exec rspec"

  concurrencies.split(',').each do |concurrency|
    target = :"collect-#{concurrency}"
    commands[target] = [] unless commands.key?(target)

    results_dir = File.join(directory, language, framework, '.results', concurrency)
    saturation_out = File.join(results_dir, 'saturation.json')
    saturation_state = File.join(results_dir, '.saturation-state.json')
    zrk_cmds = []
    routes.each do |method, uri|
      output = File.join(directory, language, framework, '.results', concurrency, "#{uri.tr('/', '_')}.json")
      request = BenchmarkWorkloads.request(method, uri, root: directory)
      args = ['--connections', concurrency, '--duration', duration, '--timeout', '8s', '--format', 'json', *request[:options]].shelljoin
      url = "http://`cat #{Shellwords.escape(hostname)}`:3000#{Shellwords.escape(uri)}"
      script_arguments = request[:arguments].shelljoin
      zrk_cmds << "#{zrk} #{args} --output #{Shellwords.escape(output)} #{url} #{script_arguments}".rstrip
      next if latency_rate.empty?

      # Resolve percentage rates from the completed throughput run.
      latency_output = File.join(results_dir, "#{uri.tr('/', '_')}_latency.json")
      rate = "`ruby #{latency_probe} --closed #{output} --spec #{latency_rate}`"
      zrk_cmds << "#{taskset}zrk #{args} --plain -t #{threads} -R #{rate} --output #{Shellwords.escape(latency_output)} #{url} #{script_arguments}".rstrip
    end

    # Sample CPU usage before and after collection.
    commands[target] << "ruby #{saturation_probe} --cid #{cid_file} --start --state #{saturation_state}"

    commands[target] << zrk_cmds.join(' && ')

    commands[target] << "ruby #{saturation_probe} --cid #{cid_file} --stop --state #{saturation_state} --out #{saturation_out}"
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
  # Rendering compiler flags must not alter the next engine's configuration.
  config = config.merge('language' => config['language'].dup)
  path = File.join(Dir.pwd, directory, '..', "#{engine}.Dockerfile")
  path = File.readlink(path) if File.symlink?(path)
  path = File.join(Dir.pwd, directory, '..', 'Dockerfile') unless File.exist?(path)

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
  config.merge!(template_variables).merge!({ if: template_conditions }).merge!(files:, static_files:, environment: config['environment']&.map do |k, v|
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
  source_configs = ['../../config.yaml', '../config.yaml', 'config.yaml'].map do |path|
    YAML.safe_load_file(File.join(directory, path))
  end

  engines.each do |engine|
    engine.each do |name, data|
      variables = custom_config(language_config, framework_config, data)
      variables['bootstrap'] = bootstrap_commands(source_configs, name)
      variables['environment'] = environment_variables(source_configs, name)
      variables['files'].each { |f| f.prepend(directory, File::SEPARATOR) unless f.start_with?(directory) }.uniq!
      variables['static_files']&.each do |f|
        f.prepend(directory, File::SEPARATOR) unless f.start_with?(directory)
      end&.uniq!

      create_dockerfile(directory, name, config.merge(variables))
    end
  end
end

def environment_variables(configs, engine)
  configs.each_with_object({}) do |config, variables|
    language = config.fetch('language', {})
    framework = config.fetch('framework', {})
    inline_engines = Array(framework['engines']).select { |entry| entry.is_a?(Hash) && entry.key?(engine) }

    environments = [config['environment'], language['environment'], language.dig('engines', engine, 'environment'),
                    framework['environment'], *inline_engines.map { |entry| entry.dig(engine, 'environment') }]
    environments.compact.each do |environment|
      raise ArgumentError, 'environment must be a mapping of keys to values' unless environment.is_a?(Hash)

      variables.merge!(environment)
    end
  end
end

def bootstrap_commands(configs, engine)
  configs.flat_map do |config|
    language = config.fetch('language', {})
    framework = config.fetch('framework', {})
    inline_engine = Array(framework['engines']).find { |entry| entry.is_a?(Hash) && entry.key?(engine) }

    [config['bootstrap'], language['bootstrap'], language.dig('engines', engine, 'bootstrap'),
     framework['bootstrap'], inline_engine&.dig(engine, 'bootstrap')].compact.flat_map do |commands|
      raise ArgumentError, 'bootstrap must be a list of commands' unless commands.is_a?(Array) && commands.all?(String)

      commands
    end
  end
end

def create_makefile(language, framework, engines)
  path = File.join(language, framework, MANIFESTS[:build])

  File.open(path, 'w') do |makefile|
    names = engines.flat_map(&:keys)
    names.each_with_index do |engine, index|
      result = commands_for(language, framework, engine)
      result[:commands].each do |target, cmds|
        # Preserve the first engine as the default for existing callers.
        makefile.puts "#{target}: #{target}.#{engine}" if index.zero?
        prereqs = result[:prerequisites][target].map { |prereq| "#{prereq}.#{engine}" }
        makefile.puts ".PHONY: #{target} #{target}.#{engine}"
        makefile.puts "#{target}.#{engine}: #{prereqs.join(' ')}".rstrip
        cmds.each { |cmd| makefile.puts("\t#{cmd}") }
      end
    end

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
    next if File.basename(file) == 'memory.json'
    next if File.basename(file) == 'memory_idle.json'

    data = JSON.load_file(file, symbolize_names: true)
    rate = (1 - data[:error_rate].to_f).round(2)
    next unless rate < 1

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
