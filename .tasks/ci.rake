require 'json'
require 'yaml'

namespace :ci do
  # Load matrix configuration
  def load_matrix_config
    config_path = File.join(Dir.pwd, '.github', 'matrix-config.yaml')
    if File.exist?(config_path)
      YAML.safe_load(File.read(config_path), permitted_classes: [Symbol], aliases: false)
    else
      {}
    end
  end

  # Get runtime versions for a language
  def get_runtime_versions(language, matrix_config, changed_files, is_pr)
    return [nil] unless matrix_config.dig('runtimes', language)

    # Check if this is a full matrix run
    branch = ENV.fetch('GITHUB_REF_NAME', '')
    full_matrix_branches = matrix_config.dig('optimization', 'full_matrix_branches') || []
    is_full_matrix = full_matrix_branches.any? { |b| branch.end_with?(b) }

    # For PRs, use selective testing if enabled
    if is_pr && matrix_config.dig('optimization', 'selective_testing') && !is_full_matrix
      # Check if runtime trigger paths are changed
      trigger_paths = matrix_config.dig('optimization', 'runtime_trigger_paths', language) || []
      runtime_triggered = trigger_paths.any? do |pattern|
        changed_files.any? { |file| File.fnmatch(pattern, file) }
      end

      # If runtime paths not triggered and pr_default_runtime_only is true, only test default
      if !runtime_triggered && matrix_config.dig('optimization', 'pr_default_runtime_only')
        default_runtime = matrix_config.dig('default_runtimes', language)
        # For Hash types (like JavaScript), return the hash so engine-specific processing can happen
        return default_runtime if default_runtime.is_a?(Hash)
        return [default_runtime].compact
      end
    end

    # Return all runtime versions
    runtimes = matrix_config.dig('runtimes', language)
    if runtimes.is_a?(Hash)
      # For JavaScript, return nested structure
      return runtimes
    end
    runtimes || [nil]
  end

  # Get engine-specific runtime
  def get_engine_runtime(language, engine, runtime_versions)
    # For JavaScript engines (node, deno, bun)
    if runtime_versions.is_a?(Hash)
      # Map engine to runtime type
      engine_type = case engine.to_s
                    when /node/ then 'node'
                    when /deno/ then 'deno'
                    when /bun/ then 'bun'
                    else 'node'
                    end
      versions = runtime_versions[engine_type]
      return versions.is_a?(Array) ? versions : [versions].compact
    end
    
    runtime_versions.is_a?(Array) ? runtime_versions : [runtime_versions].compact
  end

  # Check if combination is excluded
  def is_excluded?(language, framework, runtime, engine, matrix_config)
    exclusions = matrix_config['exclusions'] || []
    exclusions.any? do |exclusion|
      (exclusion['language'].nil? || exclusion['language'] == language) &&
        (exclusion['framework'].nil? || exclusion['framework'] == framework) &&
        (exclusion['runtime'].nil? || exclusion['runtime'] == runtime) &&
        (exclusion['engine'].nil? || exclusion['engine'] == engine)
    end
  end

  task :matrix do
    files = JSON.parse(ENV.fetch('FILES', nil))
    matrix = { include: [] }
    matrix_config = load_matrix_config

    # Check if this is a PR
    is_pr = ENV.fetch('GITHUB_EVENT_NAME', '') == 'pull_request'

    files = Dir.glob('*/*/config.yaml') if files.include?('data.json')

    files += files
             .find_all { |path| path.end_with?('Dockerfile') || (path.split('/').size == 2 && path.end_with?('config.yaml')) }
             .map { |path| path.split(File::SEPARATOR).shift }
             .flat_map { |language| Dir.glob(File.join(language, '*', 'config.yaml')) }

    files.each do |file|
      next if file.start_with?('.')
      next if file.count(File::SEPARATOR) < 2
      next unless File.exist?(file)

      language, framework, = file.split(File::SEPARATOR)

      config = get_config_from(File.join(Dir.pwd, language, framework))

      engines = config.dig('framework', 'engines')
      next unless engines

      # Get runtime versions for this language
      runtime_versions = get_runtime_versions(language, matrix_config, files, is_pr)

      # Check for framework-specific runtime overrides
      framework_key = "#{language}/#{framework}"
      if matrix_config.dig('framework_runtime_overrides', framework_key)
        runtime_versions = matrix_config.dig('framework_runtime_overrides', framework_key)
      end

      engines.each do |engine|
        # Get engine-specific runtimes
        engine_runtimes = get_engine_runtime(language, engine, runtime_versions)
        
        engine_runtimes.each do |runtime|
          # Skip excluded combinations
          next if is_excluded?(language, framework, runtime, engine, matrix_config)

          entry = {
            language: language,
            framework: framework,
            directory: File.join(language, framework),
            engine: engine
          }

          # Add runtime if specified
          entry[:runtime] = runtime if runtime

          matrix[:include] << entry
        end
      end
    end

    matrix[:include].uniq!
    
    # Apply max parallel jobs limit
    max_jobs = matrix_config.dig('optimization', 'max_parallel_jobs') || 256
    matrix[:include] = matrix[:include].take(max_jobs)
    
    puts matrix.to_json
  end
end
