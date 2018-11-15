require "json"
require "option_parser"
require "io/memory"
require "yaml"
require "kiwi/file_store"
require "admiral"
require "ssh2"
require "dotenv"

# Load env var + raise if not found
Dotenv.load!

# Wrapper around doctl tool

def execute(cmd)
  error = IO::Memory.new
  output = IO::Memory.new

  retval = Process.run(cmd, shell: true, output: output, error: error)

  # if stderr is not empty
  if error.size > 0
    msg = error.to_s.strip
    error.close
    output.close
    raise msg
  end
  ret = JSON.parse(output.to_s.strip)
  output.close
  error.close
  ret
end

class App < Admiral::Command
  class Create < Admiral::Command
    define_flag language : String, description: "language selected, to set-up environment", required: true, short: l
    define_flag framework : String, description: "framework that will eb set-up", required: true, short: f

    # droplet configuration
    define_flag image : String, description: "droplet image / os", short: i, default: "fedora-28-x64"
    define_flag region : String, description: "droplet region", short: r, default: "fra1"
    define_flag size : String, description: "droplet size (default the cheaper)", short: s, default: "s-1vcpu-1gb"

    def run
      database = Kiwi::FileStore.new("config.db")
      config = YAML.parse(File.read("#{flags.language}/config.yml"))
      fwk_config = YAML.parse(File.read("#{flags.language}/#{flags.framework}/config.yml"))
      files = fwk_config["files"].as_a

      template = config["providers"]["digitalocean"]["config"]
      f = File.open("/tmp/template.yml", "w")
      f.puts("#cloud-config")
      f.puts(YAML.dump(template).gsub("---", "")) # cloud-init does not accepts start-comment in yaml
      f.close
      instances = execute("doctl compute droplet create #{flags.framework} --image #{flags.image} --region #{flags.region} --size #{flags.size} --ssh-keys #{ENV["SSH_FINGERPINT"]} --user-data-file /tmp/template.yml")
      instance_id = instances[0]["id"]
      ip = String.new
      # wait droplet's network to be available
      loop do
        sleep 5
        instance = execute("doctl compute droplet get #{instance_id}")
        if instance[0]["networks"].size > 0
          ip = instance[0]["networks"]["v4"][0]["ip_address"]
          break
        end
      end

      database = Kiwi::FileStore.new("config.db")
      database.set("#{flags.framework.to_s.upcase}_IP", ip.to_s)

      SSH2::Session.open(ip.to_s, 22) do |session|
        session.login_with_pubkey("root", File.expand_path(ENV["SSH_KEY"]))

        # Create directory
        session.open_session do |ch|
          ch.command("mkdir -p /usr/src/app")
          IO.copy(ch, STDOUT)
        end

        # Upload files
        files.each do |file|
          path = File.join(Dir.current, flags.language.to_s, flags.framework.to_s, file.to_s)
          session.scp_send(File.join("/usr/src/app", file.to_s), 0o0644, File.size(path)) do |ch|
            ch.puts File.read(path)
          end
        end
      end
    end
  end

  class Exec < Admiral::Command
    define_flag language : String, description: "language selected, to set-up environment", required: true, short: l
    define_flag framework : String, description: "framework that will eb set-up", required: true, short: f

    def run
      database = Kiwi::FileStore.new("config.db")
      ip = database.get("#{flags.framework.to_s.upcase}_IP")

      SSH2::Session.open(ip.to_s, 22) do |session|
        session.login_with_pubkey("root", File.expand_path(ENV["SSH_KEY"]))

        session.open_session do |ch|
          arguments.each do |cmd|
            ch.command("cd /usr/src/app && #{cmd}")
            IO.copy(ch, STDOUT)
          end
        end
      end
    end
  end

  class Delete < Admiral::Command
    define_flag language : String, description: "language selected, to set-up environment", required: true, short: l
    define_flag framework : String, description: "framework that will eb set-up", required: true, short: f

    def run
      execute("doctl compute droplet delete #{flags.framework} --force")
    end
  end

  register_sub_command create : Create, description "Create droplet for specific language"
  register_sub_command exec : Exec, description "Execute command on previously created droplet"
  register_sub_command delete : Delete, description "Delet previously created droplet"

  def run
    puts "help"
  end
end

App.run
