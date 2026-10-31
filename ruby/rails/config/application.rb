require_relative 'boot'
require_relative '../lib/middleware/upload_tempfile_cleanup'
require_relative '../lib/middleware/require_media_type'

require 'rails'
require 'action_controller/railtie'
require 'rack/bad_request'
require 'rack/config'
require 'rack/conditional_get'
require 'rack/etag'
require 'stringio'

Bundler.require(*Rails.groups)

module Benchmark
  class Application < Rails::Application
    config.load_defaults 8.1
    # Middleware instances survive reloads, so load their classes explicitly above.
    config.autoload_lib(ignore: %w[assets tasks middleware])
    config.api_only = true

    if defined?(Iodine)
      # Iodine 0.8.3 misnames CONTENT_LENGTH and its input cannot rewind normally.
      # Normalize its Rack interface so Rails can read and parse bodies itself.
      config.middleware.use Rack::Config do |env|
        env['CONTENT_LENGTH'] ||= env.delete('HTTP_CONTENT_LENGTH')
        env['rack.input'] = StringIO.new(env['rack.input'].read.to_s) if env['rack.input']
      end
    end

    # API-only Rails omits this middleware; uploads still need deterministic cleanup.
    config.middleware.use UploadTempfileCleanup
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
