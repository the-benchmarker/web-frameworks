require "admiral"
require "yaml"

struct FrameworkConfig
  property name : String
  property website : String
  property version : Float32
  property langver : Float32 | String

  def initialize(@name, @website, @version, @langver)
  end
end

alias FrameworkConfigs = Array(FrameworkConfig)

class App < Admiral::Command
  class Config < Admiral::Command
    def run
      frameworks = {} of String => FrameworkConfigs
      Dir.glob("*/*/config.yml").each do |file|
        directory = File.dirname(file)
        infos = directory.split("/")
        framework = infos.pop
        language = infos.pop
        fwk_config = YAML.parse(File.read(file))
        lng_config = YAML.parse(File.read(File.join(language, "config.yml")))
        config = lng_config.as_h.merge(fwk_config.as_h)

        # Discover documentation URL for this framework

        website = config["framework"]["website"]?
        if website.nil?
          website = "github/#{config["framework"]["github"]}"
        end
        version = config["framework"]["version"]
        langver = config["provider"]["default"]["language"]

        unless frameworks.has_key?(language)
          frameworks[language] = [] of FrameworkConfig
        end

        frameworks[language] << FrameworkConfig.new(framework, website.to_s, version.to_s.to_f32, langver.to_s)
      end

      selection = YAML.build do |yaml|
        yaml.mapping do
          frameworks.each do |language, configs|
            yaml.scalar language
            yaml.mapping do
              configs.each do |config|
                yaml.scalar config.name
                yaml.mapping do
                  yaml.scalar "website"
                  yaml.scalar "https://#{config.website}"
                  yaml.scalar "version"
                  yaml.scalar config.version
                  yaml.scalar "language"
                  yaml.scalar config.langver
                end
              end
            end
          end
        end
      end
      File.write("FRAMEWORKS.yml", selection)
    end
  end

  register_sub_command config : Config, description "Create framework list"

  def run
    puts "help"
  end
end

App.run
