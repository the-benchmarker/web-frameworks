module BenchmarkWorkloads
  V1_ROUTES = [['GET', '/'], ['GET', '/user/42'], ['POST', '/user']].freeze
  # GET / and POST /user remain required by the v2 correctness contract.
  V2_ROUTES = [
    ['GET', '/user/00000042'],
    ['GET', '/serialization'], ['POST', '/deserialization'], ['POST', '/upload']
  ].freeze

  # Lua script => routes it generates. The v1 /user/42 route keeps its fixed ID;
  # the v2 user ID script varies the ID on every request.
  ROUTES_BY_SCRIPT = {
    'default.lua' => [['GET', '/'], ['GET', '/user/42']],
    'user_post.lua' => [['POST', '/user']],
    'user_id.lua' => [['GET', %r{\A/user/[^/]+\z}]],
    'serialization.lua' => [['GET', '/serialization']],
    'deserialization.lua' => [['POST', '/deserialization']],
    'upload.lua' => [['POST', '/upload']]
  }.freeze

  def self.routes(specs: 'v2')

    return V1_ROUTES

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
