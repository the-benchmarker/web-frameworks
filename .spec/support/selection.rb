require 'yaml'

module ContractSelection
  def self.versions(language:, framework:, root: HttpHelpers::ROOT)
    config = YAML.safe_load_file(File.join(root, language, framework, 'config.yaml'))
    version = config.fetch('specs', 'v1')
    raise ArgumentError, 'specs must be v1 or v2' unless %w[v1 v2].include?(version)

    language == 'ruby' && framework == 'rails' && version == 'v2' ? %w[v1 v2] : [version]
  end
end
