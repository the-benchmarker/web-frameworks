require_relative 'boot'
require_relative '../lib/upload_tempfile_cleanup'

require 'rails'
require 'action_controller/railtie'
require 'rack/bad_request'
require 'rack/conditional_get'
require 'rack/etag'
require 'rack/deflater'

Bundler.require(*Rails.groups)

module Benchmark
  class Application < Rails::Application
    config.load_defaults 8.1
    config.autoload_lib(ignore: %w[assets tasks])
    config.api_only = true

    # API-only Rails omits this middleware; uploads still need deterministic cleanup.
    config.middleware.use UploadTempfileCleanup
    config.middleware.delete Rack::ConditionalGet
    config.middleware.delete Rack::ETag
    config.middleware.delete Rails::Rack::Logger
    # Keep all responses uncompressed, including errors and clients advertising gzip.
    config.middleware.delete Rack::Deflater

    # Preserve null array entries so the request validator can reject them.
    config.action_dispatch.perform_deep_munge = false
    config.action_controller.perform_caching = false
    config.cache_store = :null_store
    config.force_ssl = false
    config.assume_ssl = false
    config.logger = ActiveSupport::BroadcastLogger.new

    # NullStore also installs a request-local cache; remove it after initialization.
    initializer 'benchmark.disable_request_cache', after: :initialize_cache, before: :build_middleware_stack do |app|
      app.config.middleware.delete Rails.cache.middleware
    end
  end
end
