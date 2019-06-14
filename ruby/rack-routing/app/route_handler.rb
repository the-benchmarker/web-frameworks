# frozen_string_literal: true

class RouteHandler
  def initialize(env, params, url_params)
    @env = env
    @params = params
    @url_params = url_params
  end

  def get_root
    Rack::Response.new("", 200)
  end

  def get_user
    Rack::Response.new(@url_params[:id], 200)
  end

  def post_user
    Rack::Response.new("", 200)
  end

  def not_found
    Rack::Response.new("404: There is no route for your request.", 404)
  end
end
