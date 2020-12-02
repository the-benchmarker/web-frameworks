# frozen_string_literal: true

require 'yaml'

namespace :config do
  namespace :bot do
    desc 'Write dependabot config file'
    task :dependabot do
      manifests = {
        # npm: ['package.json'], bundler: ['Gemfile'], composer: ['composer.json'],
        cargo: ['Cargo.toml'],  docker: ['Dockerfile'], gomod: ['go.mod'], 
        gradle: ['build.gradle'], maven: ['pom.xml'], mix: ['mix.exs'], 
        nuget: ['web.fsproj', 'web.csproj'],
        # pip: ['requirements.txt']
      }
      config = { 'version' => 2, 'updates' => [] }
      manifests.each do |manager, filenames|
        filenames.each do |filename|
          Dir.glob("**/#{filename}").each do |file|
            config['updates'] << {
              'package-ecosystem' => manager.to_s,
              'directory' => "/#{File.dirname(file)}",
              'schedule' => { 'interval' => 'daily' }
            }
          end
        end
      end
      File.open('.github/dependabot.yml', 'w') { |f| f.write(config.to_yaml) }
    end
  end
end
