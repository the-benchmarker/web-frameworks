# frozen_string_literal: true

require "yaml"
require "mustache"
require "dotenv"
require "droplet_kit"
require "net/ssh"
require "net/scp"
require "net/http"
require "fileutils"

Dotenv.load

class ::Hash
  def recursive_merge(h)
    merge!(h) { |_key, _old, _new| _old.class == Hash ? _old.recursive_merge(_new) : _new }
  end
end

def commands_for(language, framework, **options)
  config = YAML.load(File.read("config.yaml"))

  directory = File.dirname(options[:path])
  main_config = YAML.safe_load(File.open(File.join(directory, "..", "..", "config.yaml")))
  language_config = YAML.safe_load(File.open(File.join(directory, "..", "config.yaml")))
  framework_config = YAML.safe_load(File.open(File.join(directory, "config.yaml")))
  app_config = main_config.recursive_merge(language_config).recursive_merge(framework_config)

  options[:framework] = framework
  options[:language] = language

  ENV.each do |key, value|
    options[key] = value unless options.key?(key)
  end

  commands = []

  # Compile first

  if options[:provider] != "docker" && app_config.key?("binaries")
    commands << "docker build -t #{language}.#{framework} ."
    commands << "docker run -td #{language}.#{framework} > cid.txt"
    app_config["binaries"].each do |path|
      dir = File.join(language, framework, File.dirname(path))
      FileUtils.mkdir_p(dir) unless File.exists?(dir)
      commands << "docker cp `cat cid.txt`:/opt/web/#{path} #{path}"
    end
  end

  config["providers"][options[:provider]]["build"].each do |cmd|
    commands << Mustache.render(cmd, options).to_s
  end

  config["providers"][options[:provider]]["metadata"].each do |cmd|
    commands << Mustache.render(cmd, options).to_s
  end

  unless options[:collect] == "off"
    commands << "DATABASE_URL=#{ENV["DATABASE_URL"]} ../../bin/client --language #{language} --framework #{framework} #{options[:sieger_options]}"
  end

  unless options[:clean] == "off"
    config["providers"][options[:provider]]["clean"].each do |cmd|
      commands << Mustache.render(cmd, options).to_s
    end
  end

  commands
end

def create_dockerfile(language, framework, **options)
  directory = File.join(Dir.pwd, language, framework)
  main_config = YAML.safe_load(File.open(File.join(Dir.pwd, "config.yaml")))
  language_config = YAML.safe_load(File.open(File.join(Dir.pwd, language, "config.yaml")))
  framework_config = YAML.safe_load(File.open(File.join(directory, "config.yaml")))
  config = main_config.recursive_merge(language_config).recursive_merge(framework_config)

  if config.key?("sources")
    files = []
    config["sources"].each do |path|
      Dir.glob(File.join(directory, path)).each do |f|
        if f =~ /^*\.\./
          filename = f.gsub(directory, "").gsub!(/\/\.\.\/\./, "")
          File.open(File.join(directory, filename), "w") { |stream| stream.write(File.read(f)) }
          files << filename
        else
          files << f.gsub!(directory, "").gsub!(/^\//, "")
        end
      end
    end
    config["sources"] = files
  end
  if config.key?("files")
    files = []
    config["files"].each do |path|
      Dir.glob(File.join(directory, path)).each do |f|
        if f =~ /^*\.\./
          filename = f.gsub(directory, "").gsub!(/\/\.\.\/\./, "")
          File.open(File.join(directory, filename), "w") { |stream| stream.write(File.read(f)) }
          files << filename
        else
          files << f.gsub!(directory, "").gsub!(/^\//, "")
        end
      end
    end
    config["files"] = files
  end

  template = nil
  if options[:provider] == "docker"
    template = File.join(directory, "..", "Dockerfile")
  else
    unless ["javascript", "php", "python", "ruby", "julia", "perl", "dart"].include?(language)
      template = File.join(directory, "..", ".build", options[:provider], "Dockerfile")
    end
  end

  if config.key?("environment")
    environment = []
    config.fetch("environment").each do |key, value|
      environment << "#{key} #{value}"
    end
    config["environment"] = environment
  end

  File.open(File.join(directory, "Dockerfile"), "w") { |f| f.write(Mustache.render(File.read(template), config)) } if template
end

task :config do
  provider = ENV.fetch("PROVIDER") { "docker" }

  sieger_options = ENV.fetch("SIEGER_OPTIONS") { "-r GET:/ -c 10" }
  collect = ENV.fetch("COLLECT") { "on" }
  clean = ENV.fetch("CLEAN") { "on" }

  config = { main: { depends_on: [] } }

  Dir.glob("*/*/config.yaml").each do |path|
    directory = File.dirname(path)
    language, framework = directory.split(File::Separator)

    config[:main][:depends_on] << language unless config[:main][:depends_on].include?(language)

    unless config.key?(language)
      config[language] = { depends_on: [] }
    end

    config[language][:depends_on] << "#{language}.#{framework}"

    create_dockerfile(language, framework, provider: provider)

    config["#{language}.#{framework}"] = {
      commands: commands_for(language, framework, provider: provider, clean: clean, collect: collect, sieger_options: sieger_options, path: path),
      dir: File.join(language, File::SEPARATOR, framework),
    }
  end

  File.open("neph.yaml", "w") { |f| f.write(JSON.load(config.to_json).to_yaml) }
end

namespace :cloud do
  task :config do
    language = ENV.fetch("LANG")
    framework = ENV.fetch("FRAMEWORK")
    provider = ENV.fetch("PROVIDER") { "docker" }

    directory = File.join(Dir.pwd, language, framework)
    main_config = YAML.safe_load(File.open(File.join(Dir.pwd, "config.yaml")))
    language_config = YAML.safe_load(File.open(File.join(Dir.pwd, language, "config.yaml")))
    framework_config = YAML.safe_load(File.open(File.join(directory, "config.yaml")))
    config = main_config.recursive_merge(language_config).recursive_merge(framework_config)

    if config.key?("service")
      config["cloud"]["config"]["write_files"] = [{
        "path" => "/lib/systemd/system/web.service",
        "permission" => "0644",
        "content" => Mustache.render(config["service"], config),
      }]
    else
      config["cloud"]["config"]["write_files"] = []
    end

    if config.key?("environment")
      environment = config.fetch("environment")
      stringified_environment = String.new
      environment.map { |k, v| stringified_environment += "#{k}=#{v}\n" }
      config["cloud"]["config"]["write_files"] << {
        "path" => "/etc/web",
        "permission" => "0644",
        "content" => stringified_environment,
      }
    else
      config["cloud"]["config"]["write_files"] << {
        "path" => "/etc/web",
        "permission" => "0644",
        "content" => "",
      }
    end

    if config.key?("deps")
      config["deps"].each do |package|
        config["cloud"]["config"]["packages"] << package
      end
    end

    if config.key?("before_command")
      commands = config["cloud"]["config"]["runcmd"] || []
      config["cloud"]["config"]["runcmd"] = []
      config["before_command"].each do |cmd|
        config["cloud"]["config"]["runcmd"] << cmd
      end
      commands.each do |cmd|
        config["cloud"]["config"]["runcmd"] << cmd
      end
    end

    if config.key?("after_command")
      config["after_command"].each do |cmd|
        config["cloud"]["config"]["runcmd"] << cmd
      end
    end

    directories = []
    if config.key?("files")
      config["files"].each do |pattern|
        path = File.join(directory, pattern)
        files = Dir.glob(path)

        files.each do |path|
          remote_path = path.gsub(directory, "").gsub(%r{^/}, "").gsub(%r{^\.\./\.}, "")
          remote_directory = File.dirname(remote_path)

          config["cloud"]["config"]["write_files"] << {
            "path" => "/opt/web/#{remote_path}",
            "content" => File.read(path),
            "permission" => "0644",
          }

          next if remote_directory.start_with?(".")
          directories << File.join("/opt/web", File::Separator, remote_directory)
        end
      end
    end

    directories.uniq!
    directories.each do |remote_directory|
      config["cloud"]["config"]["runcmd"] << "mkdir -p #{remote_directory}"
    end

    if config.key?("binaries")
      config["cloud"]["config"]["runcmd"] << "mkdir -p /opt/web/bin"
    end

    File.open(File.join(directory, "user_data.yml"), "w") do |f|
      f.write("#cloud-config")
      f.write("\n")
      f.write(config["cloud"]["config"].to_yaml)
    end
  end

  task :upload do
    language = ENV.fetch("LANG")
    framework = ENV.fetch("FRAMEWORK")

    directory = File.join(Dir.pwd, language, framework)
    main_config = YAML.safe_load(File.open(File.join(Dir.pwd, "config.yaml")))
    language_config = YAML.safe_load(File.open(File.join(Dir.pwd, language, "config.yaml")))
    framework_config = YAML.safe_load(File.open(File.join(directory, "config.yaml")))
    config = main_config.recursive_merge(language_config).recursive_merge(framework_config)

    if config.key?("binaries")
      config["binaries"].each do |pattern|
        path = File.join(directory, pattern)
        files = Dir.glob(path)

        binaries = {}
        files.each do |path|
          remote_path = path.gsub(directory, "").gsub(%r{^/}, "").gsub(%r{^\.\./\.}, "")
          binaries[path] = File.join("/opt/web", remote_path)
        end

        unless binaries.empty?
          Net::SCP.start(ENV["HOST"], "root", keys: [ENV["SSH_KEY"]]) do |scp|
            binaries.each do |local_path, remote_path|
              scp.upload!(local_path, remote_path)
            end
          end
        end
      end
    end
  end

  task :wait do
    while true
      STDOUT.puts "Tring to connect"
      begin
        ssh = Net::SSH.start(ENV["HOST"], "root", keys: [ENV["SSH_KEY"]])
      rescue Errno::ECONNREFUSED, Errno::EHOSTUNREACH
        sleep 5
        next
      else
        break
      end
    end

    while true
      output = ssh.exec!("cloud-init status")
      _, status = output.split(":")

      raise RuntimeError, "Cloud-init have failed" if status.strip == "error"

      break if status.strip == "done"

      STDOUT.puts "Cloud-init is still running"
      sleep 5
    end

    ssh.close
  end
end

namespace :ci do
  task :config do
    frameworks = []
    Dir.glob("*/*/config.yaml").each do |file|
      directory = File.dirname(file)
      infos = directory.split("/")
      framework = infos.pop
      language = infos.pop
      frameworks << "#{language}.#{framework}"
    end
    config = File.read(".ci/template.mustache")
    File.write(".travis.yml", Mustache.render(config, { "frameworks" => frameworks }))
  end
end
