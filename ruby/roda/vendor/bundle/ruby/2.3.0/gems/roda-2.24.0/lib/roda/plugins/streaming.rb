# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The streaming plugin adds support for streaming responses
    # from roda using the +stream+ method:
    #
    #   plugin :streaming
    #
    #   route do |r|
    #     stream do |out|
    #       ['a', 'b', 'c'].each{|v| out << v; sleep 1}
    #     end
    #   end
    #
    # In order for streaming to work, any webservers used in
    # front of the roda app must not buffer responses.
    #
    # The stream method takes the following options:
    #
    # :callback :: A callback proc to call when the connection is
    #              closed.
    # :keep_open :: Whether to keep the connection open after the
    #               stream block returns, default is false.
    # :loop :: Whether to call the stream block continuously until
    #          the connection is closed.
    #
    # If the :loop option is used, you can override the
    # handle_stream_error method to change how exceptions
    # are handled during streaming. This method is passed the
    # exception and the stream.  By default, this method
    # just reraises the exception, but you can choose to output
    # the an error message to the stream, before raising:
    #
    #   def handle_stream_error(e, out)
    #     out << 'ERROR!'
    #     raise e
    #   end
    #
    # Ignore errors completely while streaming:
    #
    #   def handle_stream_error(e, out)
    #   end
    #
    # or handle the errors in some other way.
    #
    # The implementation was originally taken from Sinatra,
    # which is also released under the MIT License:
    #
    # Copyright (c) 2007, 2008, 2009 Blake Mizerany
    # Copyright (c) 2010, 2011, 2012, 2013, 2014 Konstantin Haase
    # 
    # Permission is hereby granted, free of charge, to any person
    # obtaining a copy of this software and associated documentation
    # files (the "Software"), to deal in the Software without
    # restriction, including without limitation the rights to use,
    # copy, modify, merge, publish, distribute, sublicense, and/or sell
    # copies of the Software, and to permit persons to whom the
    # Software is furnished to do so, subject to the following
    # conditions:
    # 
    # The above copyright notice and this permission notice shall be
    # included in all copies or substantial portions of the Software.
    # 
    # THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    # EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
    # OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    # NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    # HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    # WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    # FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
    # OTHER DEALINGS IN THE SOFTWARE.
    module Streaming
      OPTS = {}.freeze

      # Class of the response body in case you use #stream.
      #
      # Three things really matter: The front and back block (back being the
      # block generating content, front the one sending it to the client) and
      # the scheduler, integrating with whatever concurrency feature the Rack
      # handler is using.
      #
      # Scheduler has to respond to defer and schedule.
      class Stream
        include Enumerable

        # The default scheduler to used when streaming, useful for code
        # using ruby's default threading support.
        class Scheduler
          # Store the stream to schedule.
          def initialize(stream)
            @stream = stream
          end

          # Immediately yield.
          def defer(*)
            yield
          end

          # Close the stream if there is an exception when scheduling,
          # and reraise the exception if so.
          def schedule(*)
            yield
          rescue Exception
            @stream.close
            raise
          end
        end

        # Handle streaming options, see Streaming for details.
        def initialize(opts=OPTS, &back)
          @scheduler = opts[:scheduler] || Scheduler.new(self)
          @back = back.to_proc
          @keep_open = opts[:keep_open]
          @callbacks = []
          @closed = false

          if opts[:callback]
            callback(&opts[:callback])
          end
        end

        # Add output to the streaming response body.
        def write(data)
          @scheduler.schedule{@front.call(data.to_s)}
          self
        end

        # Alias for +write+.
        def <<(data)
          write(data)
        end

        # Add the given block as a callback to call when the block closes.
        def callback(&block)
          return yield if closed?
          @callbacks << block
        end

        # Alias to callback for EventMachine compatibility.
        alias errback callback

        # If not already closed, close the connection, and call
        # any callbacks.
        def close
          return if closed?
          @closed = true
          @scheduler.schedule{@callbacks.each(&:call)}
        end

        # Whether the connection has already been closed.
        def closed?
          @closed
        end

        # Yield values to the block as they are passed in via #<<.
        def each(&front)
          @front = front
          @scheduler.defer do
            begin
              @back.call(self)
            rescue Exception => e
              @scheduler.schedule{raise e}
            end
            close unless @keep_open
          end
        end
      end

      module InstanceMethods
        # Immediately return a streaming response using the current response
        # status and headers, calling the block to get the streaming response.
        # See Streaming for details.
        def stream(opts=OPTS, &block)
          opts = opts.merge(:scheduler=>EventMachine) if !opts.has_key?(:scheduler) && env['async.callback']

          if opts[:loop]
            block = proc do |out|
              until out.closed?
                begin
                  yield(out)
                rescue => e
                  handle_stream_error(e, out)
                end
              end
            end
          end

          throw :halt, @_response.finish_with_body(Stream.new(opts, &block))
        end

        # Handle exceptions raised while streaming when using :loop
        def handle_stream_error(e, out)
          raise e
        end
      end
    end

    register_plugin(:streaming, Streaming)
  end
end
