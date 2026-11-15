require 'stringio'

class IodineRequestBody
  def initialize(app)
    @app = app
  end

  def call(env)
    env['CONTENT_LENGTH'] ||= env.delete('HTTP_CONTENT_LENGTH')
    if env['rack.input']
      body = env['rack.input'].read.to_s
      env['rack.input'] = StringIO.new(body)
      env['CONTENT_LENGTH'] ||= body.bytesize.to_s
    end
    @app.call(env)
  end
end
