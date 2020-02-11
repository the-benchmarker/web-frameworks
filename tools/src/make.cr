require "admiral"
require "yaml"
require "crustache"

alias DockerVariable = String | Array(String)

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
    define_flag without_sieger : Bool, description: "run sieger", default: false, long: "without-sieger"
    define_flag docker_options : String, description: "extra argument to docker cli", default: "", long: "docker-options"
    define_flag keep : Bool, description: "keep container after build (default : false)", default: false, long: "keep"

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
            lang_config = YAML.parse(File.read("#{language}/config.yaml"))
            dockerfile = Crustache.parse(File.read("#{language}/Dockerfile"))
            params = {} of String => DockerVariable
            tools.each do |tool|
              params = {} of String => DockerVariable
              framework_config = YAML.parse(File.read("#{language}/#{tool}/config.yaml"))

              if framework_config.as_h.has_key?("environment")
                environment = [] of String
                framework_config["environment"].as_h.each do |k, v|
                  environment << "#{k} #{v}"
                end
                params["environment"] = environment
              end

              if framework_config.as_h.has_key?("image")
                params["image"] = framework_config.as_h["image"].to_s
              end

              if framework_config.as_h.has_key?("build_opts")
                params["build_opts"] = framework_config.as_h["build_opts"].to_s
              end

              if env_vars = framework_config.as_h["env_vars"]?
                vars = [] of String
                env_vars.as_h.each do |name, value|
                  value = value == "{{cpu_count}}" ? `nproc --all` : value

                  vars << "ENV #{name.as_s.upcase} #{value}"
                end

                params["env_vars"] = vars
              end

              if framework_config.as_h.has_key?("deps")
                deps = [] of String
                framework_config["deps"].as_a.each do |dep|
                  deps << dep.to_s
                end
                params["deps"] = deps
              end

              if framework_config.as_h.has_key?("before_build")
                deps = [] of String
                framework_config["before_build"].as_a.each do |dep|
                  deps << dep.to_s
                end
                params["before_build"] = deps
              end

              if framework_config.as_h.has_key?("patch")
                deps = [] of String
                framework_config["patch"].as_a.each do |dep|
                  deps << dep.to_s
                end
                params["patch"] = deps
              end

              if framework_config.as_h.has_key?("build_deps")
                deps = [] of String
                framework_config["build_deps"].as_a.each do |dep|
                  deps << dep.to_s
                end
                params["build_deps"] = deps
              end

              if framework_config.as_h.has_key?("bin_deps")
                deps = [] of String
                framework_config["bin_deps"].as_a.each do |dep|
                  deps << dep.to_s
                end
                params["bin_deps"] = deps
              end

              if framework_config.as_h.has_key?("php_mod")
                deps = [] of String
                framework_config["php_mod"].as_a.each do |ext|
                  deps << ext.to_s
                end
                params["php_mod"] = deps
              end

              if framework_config.as_h.has_key?("arguments")
                params["arguments"] = framework_config["arguments"].to_s
              end

              if framework_config.as_h.has_key?("fixes")
                deps = [] of String
                framework_config["fixes"].as_a.each do |dep|
                  deps << dep.to_s
                end
                params["fixes"] = deps
              end

              if framework_config.as_h.has_key?("nginx_conf")
                deps = [] of String
                framework_config["nginx_conf"].as_a.each do |dep|
                  deps << dep.to_s
                end
                params["nginx_conf"] = deps
              end

              if framework_config.as_h.has_key?("php_ext")
                deps = [] of String
                framework_config["php_ext"].as_a.each do |ext|
                  deps << ext.to_s
                end
                params["php_ext"] = deps
              end

              if framework_config.as_h.has_key?("fixes")
                deps = [] of String
                framework_config["fixes"].as_a.each do |ext|
                  deps << ext.to_s
                end
                params["fixes"] = deps
              end

              if framework_config.as_h.has_key?("arguments")
                params["arguments"] = framework_config["arguments"].to_s
              end

              if framework_config.as_h.has_key?("docroot")
                params["docroot"] = framework_config["docroot"].to_s
                params["slasheddocroot"] = params["docroot"].to_s.gsub("/", "\\/")
              end

              if framework_config.as_h.has_key?("options")
                params["options"] = framework_config["options"].to_s
              end

              if framework_config.as_h.has_key?("command")
                params["command"] = framework_config["command"].to_s
              end

              if framework_config.as_h.has_key?("before_command")
                before_command = [] of String
                framework_config["before_command"].as_a.each do |cmd|
                  before_command << cmd.to_s
                end
                params["before_command"] = before_command
              end

              if framework_config.as_h.has_key?("standalone")
                params["standalone"] = framework_config["standalone"].to_s
              end

              if framework_config.as_h.has_key?("build")
                build = [] of String
                framework_config["build"].as_a.each do |cmd|
                  build << cmd.to_s
                end
                params["build"] = build
              end

              if framework_config.as_h.has_key?("clone")
                clone = [] of String
                framework_config["clone"].as_a.each do |cmd|
                  clone << cmd.to_s
                end
                params["clone"] = clone
              end

              if framework_config.as_h.has_key?("files")
                files = [] of String
                framework_config.as_h["files"].as_a.each do |file|
                  files << file.to_s
                end
                params["files"] = files
              end

              File.write("#{language}/#{tool}/Dockerfile", Crustache.render(dockerfile, params))
              yaml.scalar tool
              yaml.mapping do
                yaml.scalar "commands"
                yaml.sequence do
                  # Build container
                  yaml.scalar "docker build -t #{tool} . #{flags.docker_options}"

                  # Run container, and store IP
                  yaml.scalar "docker run -td #{tool} | xargs -i docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' {} > ip.txt"

                  # Launch sieging
                  unless flags.without_sieger
                     factor = System.cpu_count**2
                     [1, 4, 8, 16, 32].each do |i|
                       yaml.scalar "../../bin/client -l #{language} -f #{tool} -c #{factor*i} -r GET:/ -r GET:/user/0 -r POST:/user"
                     end
                  end

                  # Drop the container
                  unless flags.keep
                    yaml.scalar "docker ps -a -q  --filter ancestor=#{tool}  | xargs -r docker rm -f"
                  end
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

  class TravisConfig < Admiral::Command
    def run
      frameworks = [] of String
      Dir.glob("*/*/config.yaml").each do |file|
        info = file.split("/")
        frameworks << info[info.size - 2]
      end
      config = Crustache.parse(File.read(".ci/template.mustache"))
      File.write(".travis.yml", Crustache.render(config, {"frameworks" => frameworks}))
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
                manifest = String.new
                mapping["languages"][language].as_h.keys.each do |key|
                  file = key.to_s
                  if File.exists?("#{language}/#{tool}/#{file}")
                    manifest = file
                  end
                end
                next if manifest.chars.size == 0

                yaml.mapping do
                  yaml.scalar "package_manager"
                  yaml.scalar mapping["languages"][language][manifest]["label"]
                  yaml.scalar "update_schedule"
                  yaml.scalar mapping["languages"][language][manifest]["update_schedule"]
                  yaml.scalar "directory"
                  yaml.scalar "#{language}/#{tool}"
                  yaml.scalar "default_labels"
                  yaml.sequence do
                    yaml.scalar "language:#{language}"
                  end
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
  register_sub_command ci_config : TravisConfig, description "Create configuration file for CI"
  register_sub_command deps_config : DependabotConfig, description "Create configuration file for deps update bot"

  def run
    puts "help"
  end
end

App.run
