# frozen_string_literal: true

require "dotenv"
require "active_support"

MANIFESTS = {
  container: ".Dockerfile",
  build: ".Makefile",
}.freeze

Dotenv.load

class ::Hash
  def recursive_merge(hash)
    merge!(hash) { |_, old, new| old.instance_of?(Hash) ? old.recursive_merge(new) : new }
  end
end

def get_config_from(directory, engines_as_list: true)
  main_config = YAML.safe_load(File.open(File.join(directory, "..", "..", "config.yaml")))

  language_config = YAML.safe_load(File.open(File.join(directory, "..", "config.yaml")))

  framework_config = YAML.safe_load(File.open(File.join(directory, "config.yaml")))

  config = main_config.recursive_merge(language_config).recursive_merge(framework_config)

  if config.dig("framework", "engines") && !engines_as_list
    config["framework"]["engines"] = config.dig("framework", "engines").map do |row|
      if row.is_a?(String) && config.dig("language", "engines", row)
        { row => config.dig("language", "engines", row) }
      else
        row
      end
    end
  end

  skippable_keys = framework_config["framework"].select { |_k, v| v.nil? }.keys
  skippable_keys.each do |skippable_key|
    config["framework"].except!(skippable_key)
    config["language"].except!(skippable_key)
  end

  config
end

def custom_config(dict1, dict2, dict3)
  keys = dict1.keys << dict2.keys << dict3.keys
  data = {}
  keys.flatten!.uniq.each do |key|
    next if %w[version engines website github].include?(key)

    data[key] = override_or_merge(dict3[key], dict2[key], dict1[key])
  end
  data
end

def override_or_merge(value3, value2, value1)
  value = value3
  if value
    if value2
      case value2
      when Array
        value.unshift(*value2)
      when String
        value = value2
      end
    end
  else
    value = value2
  end
  if value
    if value1
      case value1
      when Array
        value.unshift(*value1).uniq!
      when String
        value = value1
      end
    end
  else
    value = value1
  end

  value
end

def commands_for(language, framework, variant, provider = "docker")
  config = YAML.safe_load(File.read("config.yaml"))

  directory = Dir.pwd
  main_config = YAML.safe_load(File.open(File.join(directory, "config.yaml")))
  language_config = YAML.safe_load(File.open(File.join(directory, language, "config.yaml")))
  framework_config = YAML.safe_load(File.open(File.join(directory, language, framework, "config.yaml")))
  app_config = main_config.recursive_merge(language_config).recursive_merge(framework_config)
  options = { language: language, framework: framework, variant: variant,
              manifest: "#{MANIFESTS[:container]}.#{variant}" }
  commands = { build: [], collect: [], clean: [] }

  # Compile first, only for non containers
  if app_config.key?("binaries") && !(provider.start_with?("docker") || provider.start_with?("podman"))
    commands << "docker build -f #{MANIFESTS[:container]}.#{variant} -t #{language}.#{framework} ."
    commands << "docker run -td #{language}.#{framework} > cid.txt"
    app_config["binaries"].each do |out|
      if out.count(File::Separator).positive?
        FileUtils.mkdir_p(File.join(directory, File.dirname(out)))
        commands[:build] << "docker cp `cat cid-#{variants}.txt`:/opt/web/#{File.dirname(out)} ."
      else
        commands[:build] << "docker cp `cat cid-#{variants}.txt`:/opt/web/#{out} #{out}"
      end
    end
  end

  config["providers"][provider]["build"].each do |cmd|
    commands[:build] << Mustache.render(cmd, options).to_s
  end

  config["providers"][provider]["metadata"].each do |cmd|
    commands[:build] << Mustache.render(cmd, options).to_s
  end

  if app_config.key?("bootstrap") && config["providers"][provider].key?("exec")
    remote_command = config["providers"][[provider]]["exec"]
    app_config["bootstrap"].each do |cmd|
      commands[:build] << Mustache.render(remote_command, options.merge!(command: cmd)).to_s
    end
  end

  if config.dig("providers", provider).key?("reboot")
    commands[:build] << config.dig("providers", provider, "reboot")
    commands[:build] << "sleep 30"
  end

  commands[:build] << "curl --retry 5 --retry-delay 5 --retry-max-time 180 --retry-connrefused http://`cat #{language}/#{framework}/ip-#{variant}.txt`:3000 -v"

  commands[:collect] << "HOSTNAME=`cat #{language}/#{framework}/ip-#{variant}.txt` ENGINE=#{variant} LANGUAGE=#{language} FRAMEWORK=#{framework} DATABASE_URL=#{ENV.fetch(
    "DATABASE_URL", nil
  )} bundle exec rake collect"

  config.dig("providers", provider, "clean").each do |cmd|
    commands[:clean] << Mustache.render(cmd, options).to_s
  end

  commands
end

def create_dockerfile(directory, engine, config)
  path = File.join(Dir.pwd, directory, "..", "#{engine}.Dockerfile")
  path = File.readlink(path) if File.symlink?(path)
  path = File.join(Dir.pwd, directory, "..", "Dockerfile") unless File.exist?(path)

  # Path to remove stability suffix (stable, beta, alpha, or version) of php extensions

  files = []

  Dir.glob(config["files"]).each do |file|
    variant_file = file.gsub(directory, File.join(directory, ".#{engine}"))

    target = if file.include?(".#{engine}")
        file.gsub(".#{engine}/", "").gsub("#{directory}/", "")
      else
        file.gsub("#{directory}/", "")
      end

    source = if File.exist?(variant_file)
        variant_file
      else
        file
      end

    files << { source: source.gsub("#{directory}/", ""), target: target }
  end

  static_files = []

  if config["static_files"]
    Dir.glob(config["static_files"]).each do |static_file|
      static_files << { source: static_file.gsub("#{directory}/", ""), target: static_file.gsub("#{directory}/", "") }
    end
  end

  template = File.read(path)
  File.write(File.join(directory, ".Dockerfile.#{engine}"), Mustache.render(template, config.merge("files" => files, "static_files" => static_files, "environment" => config["environment"]&.map do |k, v|
                                                                                                     "#{k}=#{v}"
                                                                                                   end)))
end

desc "Create Dockerfiles"
task :config do
  Dir.glob("*/*/config.yaml").each do |path|
    directory = File.dirname(path)
    config = get_config_from(directory, engines_as_list: false)

    language_config = config["language"]
    framework_config = config["framework"]
    config.dig("framework", "engines")&.each do |engine|
      engine.each do |name, data|
        variables = custom_config(language_config, framework_config, data)
        variables["files"].each { |f| f.prepend(directory, File::SEPARATOR) unless f.start_with?(directory) }.uniq!
        variables["static_files"]&.each do |f|
          f.prepend(directory, File::SEPARATOR) unless f.start_with?(directory)
        end&.uniq!

        create_dockerfile(directory, name, config.merge(variables))
      end
    end

    language, framework = directory.split(File::SEPARATOR)

    makefile = File.open(File.join(language, framework, MANIFESTS[:build]), "w")

    engine = config.dig("framework", "engines").first.first.first

    commands_for(language, framework, engine).each do |target, commands|
      makefile.write("#{target}:\n")
      commands.each do |command|
        makefile.write("\t #{command}\n")
      end
    end

    names = config.dig("framework", "engines")&.flat_map(&:keys)
    command = names&.flat_map { |n| ["build.#{n}", "collect.#{n}", "clean.#{n}"] }&.join(" ")

    makefile.write("run-all : #{command}\n")

    makefile.close
  end
end

desc "Clean unused file"
task :clean do
  Dir.glob("*/*/.gitignore").each do |ignore_file|
    directory = File.dirname(ignore_file)

    File.foreach(ignore_file) do |line|
      line.strip!
      next if line.start_with?("!")
      next if line.start_with?("#")
      next if line.start_with?(".env")
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

Dir.glob(".tasks/*.rake").each { |r| load r }
