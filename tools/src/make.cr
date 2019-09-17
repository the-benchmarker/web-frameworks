require "admiral"
require "yaml"

struct FrameworkConfig
  property name : String
  property website : String
  property version : String
  property langver : String

  def initialize(@name, @website, @version, @langver)
  end
end

class App < Admiral::Command
  class Config < Admiral::Command
    def run
      frameworks = {} of String => Array(FrameworkConfig)
      Dir.glob("*/*/config.yaml").each do |file|
        directory = File.dirname(file)
        infos = directory.split("/")
        framework = infos.pop
        language = infos.pop
        fwk_config = YAML.parse(File.read(file))
        lng_config = YAML.parse(File.read(File.join(language, "config.yaml")))
        config = lng_config.as_h.merge(fwk_config.as_h)

        # Discover documentation URL for this framework

        website = config["framework"]["website"]?
        if website.nil?
          website = "github.com/#{config["framework"]["github"]}"
        end
        version = config["framework"]["version"]
        langver = config["provider"]["default"]["language"]

        unless frameworks.has_key?(language)
          frameworks[language] = [] of FrameworkConfig
        end

        if m = version.to_s.match /^(\d+)\.(\d+)$/
          version = "#{m[1]}.#{m[2]}"
        end
        if m = langver.to_s.match /^(\d+)\.(\d+)$/
          langver = "#{m[1]}.#{m[2]}"
        end

        frameworks[language] << FrameworkConfig.new(framework, website.to_s, version.to_s, langver.to_s)
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
                  yaml.scalar " #{config.version}"
                  yaml.scalar "language"
                  yaml.scalar " #{config.langver.to_s}"
                end
              end
            end
          end
        end
      end
      File.write("FRAMEWORKS.yaml", selection)
    end
  end

  class TravisConfig < Admiral::Command
    def run
      frameworks = [] of String
      languages = [] of String
      mapping = YAML.parse(File.read(".ci/mapping.yml"))
      Dir.glob("*/*/config.yaml").each do |file|
        frameworks << file.split("/")[-2]
        languages << file.split("/")[-3]
      end
      selection = YAML.build do |yaml|
        yaml.mapping do
          yaml.scalar "jobs"
          yaml.mapping do
            yaml.scalar "include"
            yaml.sequence do
              frameworks.sort.each do |framework|
                begin
                  yaml.mapping do
                    yaml.scalar "stage"
                    yaml.scalar "test"
                    yaml.scalar "script"
                    yaml.scalar "bash .ci/test.sh"
                    yaml.scalar "language"
                    yaml.scalar "crystal"
                    yaml.scalar "env"
                    yaml.scalar "FRAMEWORK=#{framework}"
                    yaml.scalar "services"
                    yaml.scalar "docker"
                  end
                end
              end
            end
          end
          yaml.scalar "notifications"
          yaml.mapping do
            yaml.scalar "email"
            yaml.scalar false
          end
          yaml.scalar "before_install"
          yaml.scalar "bash .ci/has_to_run.sh || travis_terminate 0"
          yaml.scalar "dist"
          yaml.scalar "bionic"
        end
      end

      File.write(".travis.yml", selection)
    end
  end

  class NephConfig < Admiral::Command
    def run
      frameworks = {} of String => Array(String)
      Dir.glob("*/*/config.yaml").each do |file|
        directory = File.dirname(file)
        infos = directory.split("/")
        framework = infos.pop
        language = infos.pop

        unless frameworks.has_key?(language)
          frameworks[language] = [] of String
        end

        frameworks[language] << framework
      end

      selection = YAML.build do |yaml|
        yaml.mapping do
          yaml.scalar "main"
          yaml.mapping do
            yaml.scalar "depends_on"
            yaml.sequence do
              frameworks.each do |language, _|
                yaml.scalar language
              end
            end
          end
          frameworks.each do |language, tools|
            yaml.scalar language
            yaml.mapping do
              yaml.scalar "depends_on"
              yaml.sequence do
                tools.each do |tool|
                  yaml.scalar tool
                end
              end
            end
          end
          frameworks.each do |language, tools|
            tools.each do |tool|
              yaml.scalar tool
              yaml.mapping do
                yaml.scalar "commands"
                yaml.sequence do
                  yaml.scalar "docker build -t #{tool} ."
                end
                yaml.scalar "dir"
                yaml.scalar "#{language}/#{tool}"
              end
            end
          end
        end
      end
      File.write("neph.yaml", selection)
    end
  end

  class DependabotConfig < Admiral::Command
    def run
      mapping = YAML.parse(File.read(".dependabot/mapping.yaml"))
      frameworks = {} of String => Array(String)
      Dir.glob("*/*/config.yaml").each do |file|
        directory = File.dirname(file)
        infos = directory.split("/")
        framework = infos.pop
        language = infos.pop

        unless frameworks.has_key?(language)
          frameworks[language] = [] of String
        end

        frameworks[language] << framework
      end
      selection = YAML.build do |yaml|
        yaml.mapping do
          yaml.scalar "version"
          yaml.scalar 1
          yaml.scalar "update_configs"

          yaml.sequence do
            frameworks.each do |language, tools|
              tools.each do |tool|

                # Exist if not exist for @dependabot
                next unless mapping["languages"].as_h[language]?

                # Exist if no manifest file
                manifest = mapping["languages"][language]["manifest"].to_s                
                next unless File.exists?("#{language}/#{tool}/#{manifest}")
                
                language = "javascript" if language == "node" # FIXME
                
                  yaml.mapping do
                    yaml.scalar "package_manager"
                    yaml.scalar mapping["languages"][language]["label"]
                    yaml.scalar "update_schedule"
                    yaml.scalar mapping["languages"][language]["update_schedule"]
                    yaml.scalar "directory"
                    if language == "javascript"
                      yaml.scalar "node/#{tool}"
                    else
                      yaml.scalar "#{language}/#{tool}"
                    end
                    yaml.scalar "default_labels"
                    yaml.sequence do
                      yaml.scalar "language:#{language}"
                    end
                end
              end
              language = "node" if language == "javascript" # FIXME
              directory = "#{language}/#{frameworks[language].first}"
              yaml.mapping do
                yaml.scalar "package_manager"
                yaml.scalar "docker"
                yaml.scalar "update_schedule"
                yaml.scalar "daily"
                yaml.scalar "directory"
                yaml.scalar directory
                yaml.scalar "default_labels"
                yaml.sequence do
                  yaml.scalar "docker"
                end
              end
            end
          end
        end
      end
      File.write(".dependabot/config.yml", selection)
    end
  end

  register_sub_command config : Config, description "Create framework list"
  register_sub_command neph_config : NephConfig, description "Create neph build tool configuration file"
  register_sub_command ci_config : TravisConfig, description "Create configuration file for CI"
  register_sub_command deps_config : DependabotConfig, description "Create configuration file for deps update bot"

  def run
    puts "help"
  end
end

App.run
