module BenchmarkWorkloads
  V1_ROUTES = [['GET', '/'], ['GET', '/user/42'], ['POST', '/user']].freeze
  V2_ROUTES = [
    ['GET', '/'], ['GET', '/user/00000042'], ['POST', '/user'],
    ['GET', '/serialization'], ['POST', '/deserialization'], ['POST', '/upload']
  ].freeze

  # Lua script => routes it generates. The v1 /user/42 route keeps its fixed ID;
  # the v2 user ID script varies the ID on every request.
  ROUTES_BY_SCRIPT = {
    'default.lua' => [['GET', '/'], ['GET', '/user/42'], ['POST', '/user']],
    'user_id.lua' => [['GET', %r{\A/user/[^/]+\z}]],
    'serialization.lua' => [['GET', '/serialization']],
    'deserialization.lua' => [['POST', '/deserialization']],
    'upload.lua' => [['POST', '/upload']]
  }.freeze

  def self.routes(specs: 'v2')
    raise ArgumentError, 'specs must be v1 or v2' unless %w[v1 v2].include?(specs)

    return specs == 'v1' ? V1_ROUTES : V2_ROUTES unless ENV.key?('ROUTES')

    ENV.fetch('ROUTES').split(',').map do |route|
      method, path = route.split(':', 2)
      unless method && path&.start_with?('/')
        raise ArgumentError,
              "Invalid route #{route.inspect}; expected METHOD:/path"
      end

      [method.upcase, path]
    end
  end

  def self.request(method, uri, root: Dir.pwd)
    path = uri.split('?', 2).first
    script = ROUTES_BY_SCRIPT.find do |_, routes|
      routes.any? { |route_method, route_path| method == route_method && route_path === path }
    end&.first || 'default.lua'

    options = ['--method', method, '--script', File.join(root, '.load_scripts', script)]
    { options: options, arguments: [root] }
  end
end
