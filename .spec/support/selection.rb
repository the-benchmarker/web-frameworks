require 'yaml'

module ContractSelection
  def self.version(language:, framework:, root: HttpHelpers::ROOT)
    config = YAML.safe_load_file(File.join(root, language, framework, 'config.yaml'))
    version = config.fetch('specs', 'v2')
    raise ArgumentError, 'specs must be v1 or v2' unless %w[v1 v2].include?(version)

    version
  end
end
