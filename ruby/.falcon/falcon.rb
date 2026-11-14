# frozen_string_literal: true

require "falcon/environment/rack"
require "etc"

hostname = File.basename(__dir__)
worker_count = ENV.fetch("WEB_CONCURRENCY") { Etc.nprocessors }

service hostname do
	include Falcon::Environment::Rack

	preload "preload.rb" if File.file?("preload.rb")

	count worker_count.to_i
	port {ENV.fetch("PORT", 3000).to_i}

	endpoint do
		Async::HTTP::Endpoint
			.parse("http://0.0.0.0:#{port}")
			.with(protocol: Async::HTTP::Protocol::HTTP11)
	end
end
