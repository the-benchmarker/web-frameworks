require_relative 'support/http_helpers'
require_relative 'support/selection'

version = ContractSelection.version(language: ENV.fetch('LANGUAGE'), framework: ENV.fetch('FRAMEWORK'))

# Loading support and steps explicitly keeps the version directories independent.
args = ['bundle', 'exec', 'cucumber', '--publish-quiet',
        '--require', File.join(__dir__, 'support'),
        '--require', File.join(__dir__, 'step_definitions')]
exec(*args, File.join(__dir__, version), *ARGV)
