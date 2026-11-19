require_relative "camp"

App.make_camp
run lambda { |env|
  # Iodine supplies a nil session, which bypasses Rack's default empty hash.
  env['rack.session'] ||= {}
  App.call(env)
}
