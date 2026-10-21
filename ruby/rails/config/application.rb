require_relative 'boot'
require_relative '../lib/json_body_length'
require_relative '../lib/upload_tempfile_cleanup'

require 'rails'
require 'action_controller/railtie'
require 'rack/bad_request'
require 'rack/conditional_get'
require 'rack/etag'

Bundler.require(*Rails.groups)

module Benchmark
  class Application < Rails::Application
    config.load_defaults 8.1
    config.autoload_lib(ignore: %w[assets tasks])
    config.api_only = true

    # API-only Rails omits this middleware; uploads still need deterministic cleanup.
    config.middleware.use UploadTempfileCleanup
    config.middleware.use JsonBodyLength
    config.middleware.delete Rack::ConditionalGet
    config.middleware.delete Rack::ETag
    config.middleware.delete Rack::Sendfile
    config.middleware.delete Rack::Runtime
    config.middleware.delete ActionDispatch::RequestId

    # Preserve null array entries so the request validator can reject them.
    config.action_dispatch.perform_deep_munge = false
    config.action_controller.perform_caching = false
    config.cache_store = :null_store

    unless ENV.fetch('RAILS_LOG_ENABLED', 'false') == 'true'
      config.middleware.delete Rails::Rack::Logger
      config.log_level = :unknown
      config.logger = ActiveSupport::Logger.new(nil)
    end

    # NullStore also installs a request-local cache; remove it after initialization.
    initializer 'benchmark.disable_request_cache', after: :initialize_cache, before: :build_middleware_stack do |app|
      app.config.middleware.delete Rails.cache.middleware
    end
  end
end
