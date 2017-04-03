# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The halt plugin augments the standard request +halt+ method to allow the response
    # status, body, or headers to be changed when halting.
    #
    # After loading the halt plugin:
    #
    #   plugin :halt
    #
    # You can call the halt method with an integer to set the response status and return:
    #   
    #   route do |r|
    #     r.halt(403)
    #   end
    #
    # Or set the response body and return:
    #
    #   route do |r|
    #     r.halt('body')
    #   end
    #
    # Or set both:
    #
    #   route do |r|
    #     r.halt(403, 'body')
    #   end
    #
    # Or set response status, headers, and body:
    #
    #   route do |r|
    #     r.halt(403, {'Content-Type'=>'text/csv'}, 'body')
    #   end
    #
    # As supported by default, you can still pass an array which contains a rack response:
    #
    #   route do |r|
    #     r.halt([403, {'Content-Type'=>'text/csv'}, ['body']])
    #   end
    #
    # Note that there is a difference between providing status, headers, and body as separate
    # arguments and providing them as a single rack response array.  With a rack response array,
    # the values are used directly, while with 3 arguments, the headers given are merged into
    # the existing headers and the given body is written to the existing response body.
    #
    # If using other plugins that recognize additional types of match block responses, such
    # as +symbol_views+ and +json+, you can pass those additional types to +r.halt+:
    #
    #   plugin :halt
    #   plugin :symbol_views
    #   plugin :json
    #   route do |r|
    #     r.halt(:template)
    #     r.halt(500, [{'error'=>'foo'}])
    #     r.halt(500, 'header=>'value', :other_template)
    #   end
    #
    # Note that when using the +json+ plugin with the +halt+ plugin, you cannot return a
    # array as a single argument and have it be converted to json, since it would be interpreted 
    # as a rack response.  You must use call +r.halt+ with either two or three argument forms 
    # in that case.
    module Halt
      module RequestMethods
        # Expand default halt method to handle status codes, headers, and bodies.  See Halt.
        def halt(*res)
          case res.length
          when 0 # do nothing
          when 1
            case v = res[0]
            when Integer
              response.status = v
            when Array
              throw :halt, v
            else
              if result = block_result_body(v)
                response.write(result)
              else
                raise Roda::RodaError, "singular argument given to #halt not handled: #{v.inspect}"
              end
            end
          when 2
            resp = response
            resp.status = res[0]
            resp.write(block_result_body(res[1]))
          when 3
            resp = response
            resp.status = res[0]
            resp.headers.merge!(res[1])
            resp.write(block_result_body(res[2]))
          else
            raise Roda::RodaError, "too many arguments given to #halt (accepts 0-3, received #{res.length})"
          end

          super()
        end
      end
    end

    register_plugin(:halt, Halt)
  end
end
