require 'yaml'
require_relative 'support/http_helpers'

RSpec.configure do |config|
  config.include HttpHelpers

  specs = if ENV['LANGUAGE'] && ENV['FRAMEWORK']
            path = File.join(HttpHelpers::ROOT, ENV.fetch('LANGUAGE'), ENV.fetch('FRAMEWORK'), 'config.yaml')
            YAML.safe_load_file(path).fetch('specs', 'v1')
          else
            'v1'
          end
  raise ArgumentError, 'specs must be v1 or v2' unless %w[v1 v2].include?(specs)

  suite_path = %r{/\.spec/(v[12])/}
  config.define_derived_metadata(file_path: suite_path) do |metadata|
    metadata[:specs] = metadata[:file_path][suite_path, 1]
  end
  config.filter_run_excluding specs: specs == 'v2' ? 'v1' : 'v2'

  config.mock_with :rspec do |mocks|
    mocks.verify_partial_doubles = true
  end
end
