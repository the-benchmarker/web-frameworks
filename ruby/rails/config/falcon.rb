require 'etc'
require 'falcon/environment/rack'

service 'benchmark' do
  include Falcon::Environment::Rack

  root File.expand_path('..', __dir__)
  count Integer(ENV.fetch('WEB_CONCURRENCY') { Etc.nprocessors })

  endpoint do
    Async::HTTP::Endpoint.parse("http://0.0.0.0:#{ENV.fetch('PORT', 3000)}", reuse_address: true)
  end

  # Falcon normally adds response compression outside the Rack application.
  middleware do
    Protocol::Rack::Adapter.new(rack_app)
  end
end
