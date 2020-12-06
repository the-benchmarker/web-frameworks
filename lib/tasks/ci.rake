# frozen_string_literal: true

namespace :ci do
  task :config do
    blocks = [{ name: 'setup', dependencies: [], task: {
      jobs: [{
        name: 'setup',
        commands: [
          'checkout',
          'cache store $SEMAPHORE_GIT_SHA .',
          'sudo snap install crystal --classic',
          'sudo apt-get -y install libyaml-dev libevent-dev',
          'shards build --static',
          'cache store bin bin',
          'bundle config path .cache',
          'bundle install',
          'cache store built-in .cache',
          'bundle exec rake config'
        ]
      }]
    } }]
    Dir.glob('*/config.yaml').each do |path|
      language, = path.split(File::Separator)

      block = { name: language, dependencies: ['setup'], task: { prologue: { commands: [
        'cache restore $SEMAPHORE_GIT_SHA',
        'cache restore bin',
        'cache restore built-in',
        'find bin -type f -exec chmod +x {} \\;',
        'bundle config path .cache',
        'bundle install',
        'bundle exec rake config'
      ] }, jobs: [] } }
      Dir.glob("#{language}/*/config.yaml") do |file|
        _, framework, = file.split(File::Separator)
        block[:task][:jobs] << { name: framework, commands: [
          "cd #{language}/#{framework} && make build  -f #{MANIFESTS[:build]}  && cd -",
          "FRAMEWORK=#{language}/#{framework} bundle exec rspec .spec"
        ] }
      end
      blocks << block
    end

    config = { version: 'v1.0', name: 'Benchmarking suite', execution_time_limit: { hours: 24 }, agent: { machine: { type: 'e1-standard-2', os_image: 'ubuntu1804' } }, blocks: blocks }
    File.write('.semaphore/semaphore.yml', JSON.parse(config.to_json).to_yaml)
  end
  task :matrix do
    matrix = { include: [ { directory: 'ruby/rails', framework: 'ruby/rails'}]}
    puts matrix.to_json
  end
end
