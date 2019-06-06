require "admiral"
require "yaml"
require "crustache"

struct FrameworkConfig
  property name : String
  property website : String
  property version : Float32
  property langver : Float32 | String

  def initialize(@name, @website, @version, @langver)
  end
end

class App < Admiral::Command
  class Config < Admiral::Command
    def run
      frameworks = {} of String => Array(FrameworkConfig)
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
          website = "github.com/#{config["framework"]["github"]}"
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

  class TravisConfig < Admiral::Command
    def run
      frameworks = [] of String
      Dir.glob("*/*/config.yml").each do |file|
        frameworks << file.split("/")[-2]
      end
      template = Crustache.parse(File.read("tools/template/travis.mustache"))
      File.write(".travis.yml", Crustache.render template, {"frameworks" => frameworks})
    end
  end

  class NephConfig < Admiral::Command
    def run
      frameworks = {} of String => Array(String)
      Dir.glob("*/*/config.yml").each do |file|
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

  register_sub_command config : Config, description "Create framework list"
  register_sub_command neph_config : NephConfig, description "Create neph build tool configuration file"
  register_sub_command ci_config : TravisConfig, description "Create configuration file for CI"

  def run
    puts "help"
  end
end

App.run
