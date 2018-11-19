require "admiral"
require "dotenv"

def exec(cmd)
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

  output.close
  error.close
end

class Configure < Admiral::Command
  define_flag provider : String, default: "docker"

  def run
    Dotenv.load!

    if Process.find_executable("wrk").nil?
      raise "<wrk> is not installed"
    end

    if flags.provider == "digitalocean"
      if Process.find_executable("doctl").nil?
        raise "<doctl> is not installed"
      end

      exec("doctl auth init -t #{ENV["DO_TOKEN"]} -o json")
    end
  end
end

Configure.run
