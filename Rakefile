# frozen_string_literal: true

require "yaml"
require "base64"
require "mustache"
require "dotenv"
require "droplet_kit"

class ::Hash
  def recursive_merge(h)
    merge!(h) { |_key, _old, _new| _old.class == Hash ? _old.recursive_merge(_new) : _new }
  end
end

namespace :make do
  namespace :cloud do
    task :config do
      language = ENV.fetch("LANG")
      framework = ENV.fetch("FRAMEWORK")

      directory = File.join(language, framework)
      main_config = YAML.safe_load(File.open(File.join(directory, "..", "..", "config.yaml")))
      language_config = YAML.safe_load(File.open(File.join(directory, "..", "config.yaml")))
      framework_config = YAML.safe_load(File.open(File.join(directory, "config.yaml")))
      config = main_config.recursive_merge(language_config).recursive_merge(framework_config)

      STDOUT.puts "Creating config for #{framework} in #{language}"

      config["cloud"]["config"]["write_files"] = [{
        "path" => "/lib/systemd/system/web.service",
        "permission" => "0644",
        "content" => Mustache.render(config["service"], config),
      }]

      if config.key?("environment")
        environments = config.fetch("environment")
        config["cloud"]["config"]["write_files"] << {
          "path" => "/etc/web",
          "permission" => "0644",
          "content" => environments,
        }
      else
        config["cloud"]["config"]["write_files"] << {
          "path" => "/etc/web",
          "permission" => "0644",
          "content" => "",
        }
      end

      if config.key?("build_deps")
        config["build_deps"].each do |package|
          config["cloud"]["config"]["packages"] << package
        end
      end

      config["files"].each do |pattern|
        Dir.glob(File.join(directory, pattern)).each do |path|
          content = File.read(path)
          config["cloud"]["config"]["write_files"] << {
            "path" => "/usr/src/app#{path.gsub!(directory, "")}",
            "content" => content,
            "permission" => "0644",
          }
        end
      end
      File.open(File.join(directory, "user_data.yml"), "w") { |f| f.write(config["cloud"]["config"].to_yaml) }
    end
    task :droplet do
      language = ENV.fetch("LANG")
      framework = ENV.fetch("FRAMEWORK")

      STDOUT.puts "Creating droplet for #{framework} in #{language}"
      raise "Missing environment file" unless File.exists?(".env")

      config = "#{language}/#{framework}/user_data.yml"
      name = "#{language}.#{framework}"
      region = ENV.fetch("REGION")
      image = ENV.fetch("IMAGE")
      size = ENV.fetch("SIZE")

      Dotenv.load

      user_data = "#cloud-config\n"
      user_data += File.read(config)

      client = DropletKit::Client.new(access_token: ENV["DO_TOKEN"])
      droplet = DropletKit::Droplet.new(name: name, region: region, image: image, size: size, user_data: user_data, ssh_keys: [ENV["SSH_FINGERPINT"]])
      client.droplets.create(droplet)
    end
  end
end
