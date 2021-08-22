# frozen_string_literal: true

require 'active_support/core_ext/hash/keys'
require 'yaml'

namespace :ci do
  task :config do
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
      # TODO remove this when PR is ready
      next unless language == 'python'
      definition[:blocks] << { 'name' => language, 'dependencies' => ['setup'],
                               'run' => { 'when' => "change_in(['/#{language}/','/data.json'],{pipeline_file: 'ignore'})" }, 'task' => { 'prologue' => { 'commands' => ['cache restore $SEMAPHORE_GIT_SHA', 'cache restore wrk', 'sudo install wrk /usr/local/bin', 'cache restore bin', 'cache restore built-in', 'sem-service start postgres', 'createdb -U postgres -h 0.0.0.0 benchmark', 'psql -U postgres -h 0.0.0.0 -d benchmark < dump.sql', 'bundle config path .cache', 'bundle install', 'bundle exec rake config'] }, 'jobs' => [{ 'name' => 'setup', 'commands' => ['checkout'] }] } }
      Dir.glob("#{language}/*/config.yaml") do |file|
        _, framework, = file.split(File::Separator)

        block = {
          name: framework,
          dependencies: [language],
          run: { when: "change_in('/#{language}/#{framework}','data.json')" },
          task: {
            env_vars: [{ name: 'LANGUAGE', value: language }, { name: 'FRAMEWORK', value: framework }],
            prologue: { 'commands' => ['cache restore $SEMAPHORE_GIT_SHA', 'cache restore wrk',
                                       'sudo install wrk /usr/local/bin', 'cache restore bin', 'cache restore built-in', 'sem-service start postgres', 'createdb -U postgres -h 0.0.0.0 benchmark', 'psql -U postgres -h 0.0.0.0 -d benchmark < dump.sql', 'bundle config path .cache', 'bundle install', 'bundle exec rake config'] },
            jobs: []
          }
        }

        config = get_config_from(File.join(Dir.pwd, language, framework))
        # TODO remove this before merging to master
        next unless config

        config.dig('framework', 'engines').each do |engine|
          block[:task][:jobs] << {
            name: engine,
            commands: [
              "make  -f #{language}/#{framework}/.Makefile build.#{engine}",
              'bundle exec rspec .spec',
              "make  -f #{language}/#{framework}/.Makefile collect.#{engine}",
              'bundle exec rake db:raw_export'
            ],
            env_vars: [{ name: 'ENGINE', value: engine }]
          }
          block[:task].merge!(epilogue: { commands: ['docker logs `cat ${FRAMEWORK}/cid.txt`'] })
        end
        definition[:blocks] << block
      end
    end

    File.write('.semaphore/semaphore.yml', definition.deep_stringify_keys.to_yaml)

    definition[:blocks].map { |block| block.except!(:run) }
    File.write('.semaphore/schedule.yml', definition.deep_stringify_keys.to_yaml)
  end
end
