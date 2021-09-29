# frozen_string_literal: true

require "json"

namespace :ci do
  task :config do
    blocks = [{ name: "setup", dependencies: [], task: {
      jobs: [{
        name: "setup",
        commands: [
          'checkout',
          'cache store $SEMAPHORE_GIT_SHA .',
          'sudo rm -f /etc/apt/sources.list.d/devel:kubic:libcontainers:stable.list',
          'sudo rm -f /etc/apt/sources.list.d/helm-stable-debian.list',
          'sudo apt-get update',
          'sudo apt-get install build-essential libssl-dev git -y',
          'git clone https://github.com/wg/wrk.git wrk',
          'cd wrk && make',
          'cache store wrk wrk',
          'bundle config path .cache',
          'bundle install',
          'cache store built-in .cache',
          'bundle exec rake config'
        ]
      }]
    } }]
    Dir.glob("*/config.yaml").each do |path|
      language, = path.split(File::Separator)

      block = { name: language, dependencies: ["setup"], run: { when: "change_in(['/#{language}/','/data.json'],{pipeline_file: 'ignore'})" }, task: { prologue: { commands: [
        "cache restore $SEMAPHORE_GIT_SHA",
        "cache restore wrk",
        "sudo install wrk /usr/local/bin",
        "cache restore bin",
        "cache restore built-in",
        "sem-service start postgres",
        "createdb -U postgres -h 0.0.0.0 benchmark",
        "psql -U postgres -h 0.0.0.0 -d benchmark < dump.sql",
        "bundle config path .cache",
        "bundle install",
        "bundle exec rake config",
      ] }, jobs: [] } }
      Dir.glob("#{language}/*/config.yaml").sort_by { |name| File.mtime(name) }.reverse!.each do |file|
        _, framework, = file.split(File::Separator)

        block[:task][:jobs] << { name: framework, commands: [
          "cd #{language}/#{framework} && make build  -f #{MANIFESTS[:build]}  && cd -",
          "bundle exec rspec .spec",
          "make  -f #{language}/#{framework}/#{MANIFESTS[:build]} collect",
          "bundle exec rake db:raw_export",
        ], env_vars: [
          { name: "DATABASE_URL", value: "postgresql://postgres@0.0.0.0/benchmark" },
          { name: "DURATION", value: "10" },
          { name: "CONCURRENCIES", value: "64" },
          { name: "ROUTES", value: "GET:/" },
          { name: "FRAMEWORK", value: "#{language}/#{framework}" },
        ] }
        block[:task].merge!(epilogue: { commands: ["docker logs `cat ${FRAMEWORK}/cid.txt`"] })
        break if block[:task][:jobs].count == 50
      end
      blocks << block
    end

    config = { version: "v1.0", name: "Benchmarking suite", execution_time_limit: { hours: 24 },
               agent: { machine: { type: "e1-standard-2", os_image: "ubuntu2004" } }, blocks: blocks }
    File.write(".semaphore/semaphore.yml", JSON.parse(config.to_json).to_yaml)
    # remvoe conditional run
    config[:blocks].map { |block| block.except!(:run) }
    File.write(".semaphore/schedule.yml", JSON.parse(config.to_json).to_yaml)
  end
  task :matrix do
    files = JSON.parse(ENV["FILES"])
    matrix = { include: [] }
    
    files = Dir.glob('*/*/config.yaml') if files.include?('data.json')
    
    files.each do |file|
      next if file.start_with?(".")
      language, framework, _ = file.split(File::SEPARATOR)
      matrix[:include] << { language: language, framework: framework, directory: File.join(language, framework) }
    end
    
    warn matrix.to_json
    puts matrix.to_json
  end
end
