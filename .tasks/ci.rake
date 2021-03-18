# frozen_string_literal: true

class ::Hash
  def recursive_merge(h)
    merge!(h) { |_key, _old, _new| _old.instance_of?(Hash) ? _old.recursive_merge(_new) : _new }
  end
end

def get_config_from(main_config, directory)
  language_config = YAML.safe_load(File.open(File.join(directory, '..', 'config.yaml')))

  framework_config = YAML.safe_load(File.open(File.join(directory, 'config.yaml')))

  config = main_config.recursive_merge(language_config).recursive_merge(framework_config)

  %w[engines files environment].each do |item|
    config['framework'][item] = language_config.dig('default', item) unless framework_config[item]
  end

  config
end

namespace :ci do
  task :config do
    main_config = YAML.safe_load(File.read('config.yaml'))
    definition = {
      version: 'v1.0',
      name: 'Benchmarking suite',
      agent: { machine: { type: 'e1-standard-2', os_image: 'ubuntu1804' } },
      execution_time_limit: { hours: 24 },
      blocks: [{
        name: 'setup',
        dependencies: [],
        'task' => {
          'jobs' => [{
            'name' => 'setup',
            'commands' => ['checkout', 'cache store $SEMAPHORE_GIT_SHA .', 'sudo apt-get update',
                           'sudo apt-get install build-essential libssl-dev git -y', 'git clone https://github.com/wg/wrk.git wrk', 'cd wrk && make', 'cache store wrk wrk', 'bundle config path .cache', 'bundle install', 'cache store built-in .cache', 'bundle exec rake config']
          }]
        }
      }]
    }

    Dir.glob('*/config.yaml').each do |path|
      language, = path.split(File::Separator)
      next unless language == 'ruby'

      definition[:blocks] << { 'name' => language, 'dependencies' => ['setup'],
                               'run' => { 'when' => "change_in('/#{language}/')" }, 'task' => { 'prologue' => { 'commands' => ['cache restore $SEMAPHORE_GIT_SHA', 'cache restore wrk', 'sudo install wrk /usr/local/bin', 'cache restore bin', 'cache restore built-in', 'sem-service start postgres', 'createdb -U postgres -h 0.0.0.0 benchmark', 'psql -U postgres -h 0.0.0.0 -d benchmark < dump.sql', 'bundle config path .cache', 'bundle install', 'bundle exec rake config'] }, 'jobs' => [{ 'name' => 'setup', 'commands' => ['checkout'] }] } }
      Dir.glob("#{language}/*/config.yaml") do |file|
        _, framework, = file.split(File::Separator)
        block = {
          name: framework,
          dependencies: [language],
          run: { when: "change_in('/#{language}/#{framework}')" },
          task: {
            env_vars: [{ name: 'LANGUAGE', value: language }, { name: 'FRAMEWORK', value: framework }],
            prologue: { 'commands' => ['cache restore $SEMAPHORE_GIT_SHA', 'cache restore wrk',
                                       'sudo install wrk /usr/local/bin', 'cache restore bin', 'cache restore built-in', 'sem-service start postgres', 'createdb -U postgres -h 0.0.0.0 benchmark', 'psql -U postgres -h 0.0.0.0 -d benchmark < dump.sql', 'bundle config path .cache', 'bundle install', 'bundle exec rake config'] },
            jobs: []
          }
        }

        config = get_config_from(main_config, File.join(Dir.pwd, language, framework))

        config.dig('framework', 'engines').each do |variant, _|
          block[:task][:jobs] << {
            name: variant,
            commands: [
              "make  -f #{language}/#{framework}/.Makefile build.#{variant}",
              'bundle exec rspec .spec',
              "make  -f #{language}/#{framework}/.Makefile collect.#{variant}",
              'bundle exec rake db:export'
            ],
            env_vars: [{ name: 'VARIANT', value: variant }]
          }
        end
        definition[:blocks] << block
      end
    end

    File.write('.semaphore/semaphore.yml', JSON.parse(definition.to_json).to_yaml)
  end
end
