class RequireMediaType
  def initialize(app, media_type)
    @app = app
    @media_type = media_type
  end

  def call(env)
    # Controller middleware runs before instrumentation parses parameters. A
    # before_action is too late to guarantee 415 for malformed unsupported bodies.
    if @media_type == 'application/json'
      media_type = Rack::MediaType.type(env['CONTENT_TYPE'])
      return [415, {}, []] unless json_media_type?(media_type)

      # Rails only associates its JSON parameter parser with registered MIME types.
      # Structured syntax suffixes are valid JSON media types but are not registered
      # individually, so parse them through the standard JSON handler. Do this before
      # constructing an ActionDispatch request, which caches the content type.
      env['CONTENT_TYPE'] = 'application/json' if json_suffix?(media_type)
    else
      request = ActionDispatch::Request.new(env)
      return [415, {}, []] unless request.content_mime_type == Mime::Type.lookup(@media_type)
    end

    @app.call(env)
  end

  private

  def json_media_type?(media_type)
    %w[application/json text/x-json application/jsonrequest].include?(media_type) || json_suffix?(media_type)
  end

  def json_suffix?(media_type)
    media_type&.match?(%r{\Aapplication/[!#$&^_.+\-a-z0-9]+\+json\z}i)
  end
end
