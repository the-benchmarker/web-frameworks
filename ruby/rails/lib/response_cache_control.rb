# Malformed requests can be rejected before Rails reaches controller callbacks.
class ResponseCacheControl
  def initialize(app)
    @app = app
  end

  def call(env)
    response = @app.call(env)
    response[1]['cache-control'] = 'no-store'
    response[1]['pragma'] = 'no-cache'
    response[1]['expires'] = '0'
    response[1].delete('etag')
    response[1].delete('last-modified')
    response
  end
end
