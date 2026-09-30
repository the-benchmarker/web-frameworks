require 'net/ssh'
require 'mustache'

def load_config(language, framework)
  main = YAML.safe_load_file(File.join(Dir.pwd, 'config.yaml'))
  lang = YAML.safe_load_file(File.join(Dir.pwd, language, 'config.yaml'))
  fw = YAML.safe_load_file(File.join(Dir.pwd, language, framework, 'config.yaml'))

  main.recursive_merge(lang).recursive_merge(fw)
end

def build_write_files(config, framework_dir)
  files = []

  if config.key?('service')
    files << {
      'path' => '/usr/lib/systemd/system/web.service',
      'permission' => '0644',
      'content' => Mustache.render(config['service'], config)
    }
  end

  stringified_environment = config.key?('environment') ? config['environment'].map { |k, v| "#{k}=#{v}" }.join("\n") : ''

  files << {
    'path' => '/etc/web',
    'permission' => '0644',
    'content' => stringified_environment
  }

  Array(config['files']).each do |pattern|
    Dir.glob(File.join(framework_dir, pattern)).each do |path|
      remote_path = path
                    .gsub(framework_dir, '')
                    .gsub(%r{^/}, '')
                    .gsub(%r{^\.\./\.}, '')

      files << {
        'path' => "/opt/web/#{remote_path}",
        'permission' => '0644',
        'content' => File.read(path)
      }
    end
  end

  files
end

def build_packages(config)
  Array(config.dig('cloud', 'config', 'packages')) + Array(config['deps'])
end

def build_runcmd(config)
  cmds = []

  cmds += Array(config['bootstrap'])

  cmds += Array(config['cloud']['config']['runcmd'])

  config['php_ext']&.each do |deps|
    cmds << "pecl install #{deps}"
    cmds << "echo 'extension=#{deps}' > /etc/php.d/99-#{deps}.ini"
  end

  cmds += Array(config['after_command'])

  cmds
end

namespace :cloud do
  task :config do
    language = ENV.fetch('LANG')
    framework = ENV.fetch('FRAMEWORK')

    framework_dir = File.join(Dir.pwd, language, framework)

    config = load_config(language, framework)
    cloud_config = config['cloud']['config']

    cloud_config['write_files'] = build_write_files(config, framework_dir)
    cloud_config['packages'] = build_packages(config)
    cloud_config['runcmd'] = build_runcmd(config)

    File.open(File.join(framework_dir, 'user_data.yml'), 'w') do |f|
      f.puts '#cloud-config'
      f.write cloud_config.to_yaml
    end
  end

  task :upload do
    language = ENV.fetch('LANG')
    framework = ENV.fetch('FRAMEWORK')
    hostname = ENV.fetch('HOST')
    identity_file = File.expand_path(ENV.fetch('SSH_KEY'))

    framework_dir = File.join(Dir.pwd, language, framework)
    config = load_config(language, framework)

    return unless config['binaries']

    binaries = config['binaries'].flat_map { |pattern| Dir.glob(File.join(framework_dir, pattern)) }

    warn "Trying to connect on #{hostname} with #{identity_file}"
    Net::SSH.start(hostname, 'root', keys: [identity_file]) do |ssh|
      binaries.each do |bin|
        remote_directory = File.dirname(bin).gsub!(framework_dir, '/opt/web')
        puts "Creating #{remote_directory}"
        ssh.exec!("mkdir -p #{remote_directory}")
      end
    end

    warn "Trying to connect on #{hostname} with #{identity_file}"
    Net::SCP.start(hostname, 'root', keys: [identity_file]) do |scp|
      binaries.each do |bin|
        remote_directory = File.dirname(bin).gsub!(framework_dir, '/opt/web')
        puts "Uploading #{bin} to #{remote_directory}"
        scp.upload!(bin, remote_directory, verbose: true, recursive: true)
      end
    end
  end

  task :wait do
    hostname = ENV.fetch('HOST')
    identity_file = File.expand_path(ENV.fetch('SSH_KEY'))

    ssh = nil

    warn "Trying to connect on #{hostname} with #{identity_file}"

    loop do
      ssh = Net::SSH.start(hostname, 'root', keys: [identity_file])
      break
    rescue Errno::ECONNREFUSED, Errno::EHOSTUNREACH, Net::SSH::AuthenticationFailed => e
      pp e
      sleep 5
    end

    loop do
      output = ssh.exec!('cloud-init status')
      status = output.split(':').last.strip

      raise 'Cloud-init have failed' if status == 'error'
      break if status == 'done'

      puts 'Cloud-init is still running'
      sleep 5
    end

    ssh.close
  end
end
