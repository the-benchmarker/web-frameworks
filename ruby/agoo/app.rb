# frozen_string_literal: true

require 'agoo'

Agoo::Log.configure(dir: '',
                    console: true,
                    classic: true,
                    colorize: true,
                    states: {
                      INFO: false,
                      DEBUG: false,
                      connect: false,
                      request: false,
                      response: false,
                      eval: false,
                      push: false
                    })

# For applications with minimal processing such as this benchmark it is better
# to not have multiple worker processes so worker count is set to one. Agoo
# automatically adjusts the number of read/write threads according to the
# number of connections which provides better performance than spawning new
# worker applications.
worker_count = 1
worker_count = ENV['AGOO_WORKER_COUNT'].to_i if ENV.key?('AGOO_WORKER_COUNT')
Agoo::Server.init(3000, '.', thread_count: 0, worker_count: worker_count)

# A typical Rack application performs the request de-multiplexing in the
# #call() method. Agoo supports that approach but also offers a more efficient
# approach of letting the Agoo server perform the de-multiplexing. That
# approach is used here with two separate request handlers.

# Empty response.
class Empty
  def self.call(_req)
    [200, {}, []]
  end
end

# Reflects the id as the returned value.
class Reflect
  def self.call(req)
    [200, {}, [req['PATH_INFO'][6..-1]]]
  end
end

# Since the /index.html path is for a static asset Agoo will handle that
# without involving Ruby. To have Ruby handle the /index.html static asset the
# next line can be uncommented.

Agoo::Server.handle(:GET, '/', Empty)

Agoo::Server.handle(:GET, '/user/*', Reflect)
Agoo::Server.handle(:POST, '/user', Empty)

Agoo::Server.start
