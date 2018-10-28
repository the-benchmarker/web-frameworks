require "json"
require "option_parser"
require "io/memory"
require "yaml"
require "kiwi/file_store"
require "admiral"



def execute(cmd)
  error = IO::Memory.new
  output = IO::Memory.new
  Process.run(cmd, shell: true, output: output, error: error)
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

    # ssh configuration
    define_flag key : String, description: "ssh key fingerprint", required: true, short: k
    define_flag key_file : String, description: "ssh key file", required: true

    # droplet configuration
    define_flag image : String, description: "droplet image / os", short: i, default: "fedora-28-x64"
    define_flag region : String, description: "droplet region", short: r, default: "fra1"
    define_flag size : String, description: "droplet size (default the cheaper)", short: s, default: "s-1vcpu-1gb"

    def run
      db_path = "/tmp/#{flags.language}/#{flags.framework}"
      database = Kiwi::FileStore.new(db_path)
      config = YAML.parse(File.read("#{flags.language}/config.yml"))

      template = config["providers"]["digitalocean"]["config"]
      f = File.open("/tmp/template.yml", "w")
      f.puts("#cloud-config")
      f.puts(YAML.dump(template).gsub("---", "")) # cloud-init does not accepts start-comment in yaml
      f.close
      instances = execute("doctl compute droplet create sinatra --image #{flags.image} --region #{flags.region} --size #{flags.size} --ssh-keys #{flags.key} --user-data-file /tmp/template.yml")
      instance_id = instances[0]["id"]
      sleep 1 # wait droplet's network to be available
      instance = execute("doctl compute droplet get #{instance_id}")
      ip = instance[0]["networks"]["v4"][0]["ip_address"]
      database.set("#{flags.framework.to_s.upcase}_USERNAME", "root")
      database.set("#{flags.framework.to_s.upcase}_SSHKEY", flags.key_file)
      database.set("#{flags.framework.to_s.upcase}_IP", ip)

    end
  end

  register_sub_command create : Create, description "Create droplet for specific language"

  def run
    puts "help"
  end

end

App.run


