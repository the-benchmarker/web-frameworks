# frozen_string_literal: true

class Request
  def initialize(env)
    @env = env
    @request = Rack::Request.new(env)
    @params = params_for(env)
  end

  def params_for(env)
    case env["REQUEST_METHOD"].to_sym
    when :GET
      Rack::Utils.parse_nested_query(env["QUERY_STRING"])
    when :POST, :PUT
      body = @request.body.read.to_s

      body = "{}" if body == ""

      JSON.parse(body)
    end
  end

  def response
    routing = Router.for(@env)
    @url_params = routing[:params]

    rh = RouteHandler.new(@env, @params, @url_params)
    rh.send(routing[:method])
  end
end
