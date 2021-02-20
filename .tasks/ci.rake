# frozen_string_literal: true

require "git"

namespace :ci do
  task :config do
<<<<<<< HEAD
    blocks = [{ name: 'setup', dependencies: [], task: {
=======
    ONLY_FOR = ENV["ONLY_FOR"]
    blocks = [{ name: "setup", dependencies: [], task: {
>>>>>>> d335daa5 (display mutliple variants on README)
      jobs: [{
        name: "setup",
        commands: [
          "checkout",
          "cache store $SEMAPHORE_GIT_SHA .",
          "sudo apt-get update",
          "sudo apt-get install build-essential libssl-dev git -y",
          "git clone https://github.com/wg/wrk.git wrk",
          "cd wrk && make",
          "cache store wrk wrk",
          "bundle config path .cache",
          "bundle install",
          "cache store built-in .cache",
          "bundle exec rake config",
        ],
      }],
    } }]
    Dir.glob("*/config.yaml").each do |path|
      language, = path.split(File::Separator)

      block = { name: language, dependencies: ["setup"], run: { when: "change_in('/#{language}/')" }, task: { prologue: { commands: [
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
      Dir.glob("#{language}/*/config.yaml") do |file|
        _, framework, = file.split(File::Separator)
        block[:task][:jobs] << { name: framework, commands: [
          "cd #{language}/#{framework} && make build  -f #{MANIFESTS[:build]}  && cd -",
          "FRAMEWORK=#{language}/#{framework} bundle exec rspec .spec",
          "make  -f #{language}/#{framework}/#{MANIFESTS[:build]} collect",
          "bundle exec rake db:export",
        ], env_vars: [
          { name: "DATABASE_URL", value: "postgresql://postgres@0.0.0.0/benchmark" },
          { name: "DURATION", value: "10" },
          { name: "CONCURRENCIES", value: "64" },
          { name: "ROUTES", value: "GET:/" },
        ] }
      end
      blocks << block
    end

    config = { version: "v1.0", name: "Benchmarking suite", execution_time_limit: { hours: 24 },
               agent: { machine: { type: "e1-standard-2", os_image: "ubuntu1804" } }, blocks: blocks }
    File.write(".semaphore/semaphore.yml", JSON.parse(config.to_json).to_yaml)
    # remvoe conditional run
    config[:blocks].map { |block| block.except!(:run) }
    File.write(".semaphore/schedule.yml", JSON.parse(config.to_json).to_yaml)
  end
  task :matrix do
    base = ENV["BASE_COMMIT"]
    last = ENV["LAST_COMMIT"]
    workdir = ENV.fetch("GITHUB_WORKSPACE") { Dir.pwd }
    warn "Checking for modification from #{base} to #{last}"
    git = Git.open(Dir.pwd)
    files = []
    diff = git.gtree(last).diff(base).each { |diff| files << diff.path }
    warn "Detected modified files - #{files.join(",")}"
    frameworks = []
    files.each do |file|
      if file.match(File::SEPARATOR) && !file.start_with?(".")
        parts = file.split(File::SEPARATOR)
        frameworks << parts[0..1].join(File::SEPARATOR)
      end
    end
    matrix = { include: [] }
    frameworks.uniq.each do |framework|
      matrix[:include] << { directory: framework, framework: framework }
    end
    warn matrix.to_json
    puts matrix.to_json
  end
end
