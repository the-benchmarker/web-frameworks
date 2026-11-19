# frozen_string_literal: true

require "falcon/environment/rack"
require "etc"
require "stringio"

hostname = File.basename(__dir__)
worker_count = ENV.fetch("WEB_CONCURRENCY") { Etc.nprocessors }

service hostname do
	include Falcon::Environment::Rack

	preload "preload.rb" if File.file?("preload.rb")

	count worker_count.to_i
	port {ENV.fetch("PORT", 3000).to_i}

	# Keep Falcon's optional middleware without automatic response compression.
	middleware do
		rack_application = rack_app
		app = Protocol::Rack::Adapter.new(proc do |env|
			if env["rack.input"] && !env["CONTENT_LENGTH"] && !env["HTTP_TRANSFER_ENCODING"]
				body = env["rack.input"].read.to_s
				env["rack.input"] = StringIO.new(body)
				env["CONTENT_LENGTH"] = body.bytesize.to_s
			end
			rack_application.call(env)
		end)
		logging = verbose
		caching = cache

		Protocol::HTTP::Middleware.build do
			use Falcon::Middleware::Verbose if logging
			use Async::HTTP::Cache::General if caching
			run app
		end
	end

	endpoint do
		Async::HTTP::Endpoint
			.parse("http://0.0.0.0:#{port}")
			.with(protocol: Async::HTTP::Protocol::HTTP11)
	end
end
