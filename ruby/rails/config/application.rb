require_relative 'boot'
require_relative '../lib/request_body_limit'
require_relative '../lib/upload_tempfile_cleanup'

require 'rails'
require 'active_model/railtie'
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
    config.x.complete = ENV.fetch('COMPLETE', 'false').casecmp?('true')

    config.middleware.insert_before 0, RequestBodyLimit
    # API-only Rails omits this middleware; uploads still need deterministic cleanup.
    config.middleware.use UploadTempfileCleanup
    config.middleware.delete Rack::ConditionalGet
    config.middleware.delete Rack::ETag

    # Preserve null array entries so the request validator can reject them.
    config.action_dispatch.perform_deep_munge = false
    config.action_controller.perform_caching = false
    config.action_controller.action_on_unpermitted_parameters = false
    config.cache_store = :null_store
    config.force_ssl = false
    config.assume_ssl = false
    config.logger = ActiveSupport::Logger.new(File::NULL)
  end
end
