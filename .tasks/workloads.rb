module BenchmarkWorkloads
  V2_ROUTES = [
    ['GET', '/'], ['GET', '/user/00000042'], ['POST', '/user'],
    ['GET', '/serialization'], ['POST', '/deserialization'], ['POST', '/upload']
  ].freeze

  def self.routes(specs: 'v1')
    raise ArgumentError, 'specs must be v1 or v2' unless %w[v1 v2].include?(specs)

    return specs == 'v2' ? V2_ROUTES : [['GET', '/']] unless ENV.key?('ROUTES')

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
    script = case [method, path]
             when ['GET', '/serialization'] then 'serialization.lua'
             when ['POST', '/deserialization'] then 'deserialization.lua'
             when ['POST', '/upload'] then 'upload.lua'
             else
               method == 'GET' && path.match?(%r{\A/user/[^/]+\z}) ? 'user_id.lua' : 'default.lua'
             end

    options = ['--method', method, '--script', File.join(root, '.load_scripts', script)]
    { options: options, arguments: [root] }
  end
end
