# frozen_string_literal: true

require 'yaml'
require 'base64'
require 'mustache'
require 'dotenv'
require 'droplet_kit'

class ::Hash
  def recursive_merge(h)
    merge!(h) { |_key, _old, _new| _old.class == Hash ? _old.recursive_merge(_new) : _new }
  end
end

namespace :make do
  namespace :cloud do
    task :config do
      language = ENV.fetch('LANG')
      framework = ENV.fetch('FRAMEWORK')

      directory = File.join(language, framework)
      main_config = YAML.safe_load(File.open(File.join(directory, '..', '..', 'config.yaml')))
      language_config = YAML.safe_load(File.open(File.join(directory, '..', 'config.yaml')))
      framework_config = YAML.safe_load(File.open(File.join(directory, 'config.yaml')))
      config = main_config.recursive_merge(language_config).recursive_merge(framework_config)

      config['cloud']['config']['write_files'] = [{
        'path' => '/lib/systemd/system/web.service',
        'permission' => '0644',
        'content' => Mustache.render(config['service'], config)
      }]

      if config.key?('environment')
        environment = config.fetch('environment')
        stringified_environment = String.new
        environment.map { |k, v| stringified_environment += "#{k}=#{v}\n" }
        config['cloud']['config']['write_files'] << {
          'path' => '/etc/web',
          'permission' => '0644',
          'content' => stringified_environment
        }
      else
        config['cloud']['config']['write_files'] << {
          'path' => '/etc/web',
          'permission' => '0644',
          'content' => ''
        }
      end

      if config.key?('deps') && config.key?('build_deps')
        config['build_deps'].each do |package|
          config['cloud']['config']['packages'] << package
        end
      end

      if config.key?('before_command')
        config['before_command'].each do |cmd|
          config['cloud']['config']['runcmd'] << cmd
        end
      end

      config['files'].each do |local_path|
        path = File.join(directory, local_path)
        if File.directory?(path)
          files = Dir.glob(File.join(path, '**', '*.*'))
        elsif File.file?(path)
          files = [path]
        end

        files.each do |path|
          remote_path = path.gsub(directory, '').gsub(%r{^/}, '').gsub(%r{^\.\./\.}, '')
          content = File.read(path)
          config['cloud']['config']['write_files'] << {
            'path' => "/usr/src/app/#{remote_path}",
            'content' => content,
            'permission' => '0644'
          }
        end
      end

      File.open(File.join(directory, 'user_data.yml'), 'w') do |f|
        f.write('#cloud-config')
        f.write("\n")
        f.write(config['cloud']['config'].to_yaml)
      end
    end
  end
end
