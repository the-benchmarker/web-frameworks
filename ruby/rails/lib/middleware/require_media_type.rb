class RequireMediaType
  def initialize(app, media_type)
    @app = app
    @media_type = media_type
  end

  def call(env)
    # Controller middleware runs before instrumentation parses parameters. A
    # before_action is too late to guarantee 415 for malformed unsupported bodies.
    request = ActionDispatch::Request.new(env)
    return [415, {}, []] unless request.content_mime_type == Mime::Type.lookup(@media_type)

    @app.call(env)
  end
end
