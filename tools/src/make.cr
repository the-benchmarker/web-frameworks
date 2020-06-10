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
    define_help
    define_flag without_sieger : Bool, description: "run sieger", default: false, long: "without-sieger"
    define_flag docker_options : String, description: "extra argument to docker cli", default: "", long: "docker-options"
    define_flag keep : Bool, description: "keep container after build (default : false)", default: false, long: "keep"
    define_flag driver : String, description: "driver to use", default: "docker", long: "driver", short: "d"

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
                  # Fix for vapor
                  framework_config = YAML.parse(File.read("#{language}/#{tool}/config.yaml"))
                  if framework_config.as_h["framework"].as_h.has_key?("name")
                    name = framework_config.as_h["framework"].as_h["name"]
                  else
                    name = tool
                  end
                  yaml.scalar "#{language}.#{name}"
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

              # Fix for vapor
              if framework_config.as_h["framework"].as_h.has_key?("name")
                name = framework_config.as_h["framework"].as_h["name"]
              else
                name = tool
              end

              File.write("#{language}/#{tool}/Dockerfile", Crustache.render(dockerfile, params))

              yaml.scalar "#{language}.#{name}"

              yaml.mapping do
                yaml.scalar "commands"
                yaml.sequence do
                  # Build container
                  yaml.scalar "docker build -t #{language}.#{name} . #{flags.docker_options}"

                  # Run container, and store IP
                  case flags.driver
                  when "docker"
                    yaml.scalar "docker run -td #{language}.#{name} > cid.txt"
                    yaml.scalar "docker inspect `cat cid.txt` -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' > ip.txt"
                    yaml.scalar "sleep 25"
                  when "docker-machine"
                    yaml.scalar "docker run -p 3000:3000 -d #{language}.#{name}"
                    yaml.scalar "docker-machine ip default > ip.txt"
                    yaml.scalar "sleep 25"
                  else
                    raise "unsupported provider"
                  end

                  # Launch sieging
                  unless flags.without_sieger
                    factor = System.cpu_count**2
                    command = "../../bin/client --language #{language} --framework #{name} -r GET:/ -r GET:/user/0 -r POST:/user -h `cat ip.txt`"
                    [1, 4, 8].each do |i|
                      command += " -c #{factor*i} "
                    end

                    yaml.scalar command
                  end

                  # Drop the container
                  unless flags.keep
                    yaml.scalar "docker ps -a -q  --filter ancestor=#{language}.#{name}  | xargs docker rm -f"
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
        directory = File.dirname(file)
        infos = directory.split("/")
        framework = infos.pop
        language = infos.pop
        config = YAML.parse(File.read("#{language}/#{framework}/config.yaml"))

        # Fix for vapor
        if config.as_h["framework"].as_h.has_key?("name")
          framework = config.as_h["framework"].as_h["name"]
        end

        frameworks << "#{language}.#{framework}"
      end
      config = Crustache.parse(File.read(".ci/template.mustache"))
      File.write(".travis.yml", Crustache.render(config, {"frameworks" => frameworks}))
    end
  end

  define_help
  register_sub_command config : Config, description "Create framework list"
  register_sub_command ci_config : TravisConfig, description "Create configuration file for CI"

  def run
    puts help
  end
end

App.run
