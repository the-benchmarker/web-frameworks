# frozen_string_literal: true

require 'yaml'

namespace :config do
  namespace :bot do
    desc 'Write dependabot config file'
    task :dependabot do
      REVIEWERS = YAML.safe_load(File.read('reviewers.yaml'))
      manifests = {
        cargo: ['Cargo.toml'], docker: ['Dockerfile'], gomod: ['go.mod'],
        gradle: ['build.gradle'], maven: ['pom.xml'], mix: ['mix.exs'],
        nuget: ['web.fsproj', 'web.csproj'],
        pip: ['requirements.txt'], npm: ['package.json'],
        bundler: ['Gemfile'], composer: ['composer.json']
      }
      config = { 'version' => 2, 'updates' => [] }
      c = 0
      manifests.each do |manager, filenames|
        filenames.each do |filename|
          Dir.glob("**/#{filename}").each do |file|
            c += 1

            break if c == 100

            directory = File.dirname(file)
            current = {
              'package-ecosystem' => manager.to_s,
              'directory' => "/#{directory}/",
              'schedule' => { 'interval' => 'daily' }
            }
            framework, language = directory.split(File::SEPARATOR)
            reviewers = REVIEWERS.dig(framework, language)
            current.merge!('reviewers' => reviewers.map { |r| r.prepend('@') }) if reviewers
            config['updates'] << current
          end
        end
        File.open('.github/dependabot.yml', 'w') { |f| f.write(config.to_yaml) }
      end
    end
  end
end
