# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The run_handler plugin allows r.run to take a block, which is yielded 
    # the rack response array, before it returns it as a response.
    #
    # Additionally, r.run also takes a options hash, and you can provide a
    # <tt>:not_found=>:pass</tt> option to keep routing normally if the rack
    # app returns a 404 response.
    #
    #
    #   plugin :run_handler
    #
    #   route do |r|
    #     r.on 'a' do
    #       # Keep running code if RackAppFoo doesn't return a result
    #       r.run RackAppFoo, :not_found=>:pass
    #
    #       # Change response status codes before returning.
    #       r.run(RackAppBar) do |response|
    #         response[0] = 200 if response[0] == 201
    #       end
    #     end
    #   end
    module RunHandler
      OPTS = {}.freeze

      module RequestMethods
        # If a block is given, yield the rack response array to it.  The response can
        # be modified before it is returned by the current app.
        # 
        # If the <tt>:not_found=>:pass</tt> option is given, and the rack response
        # returned by the app is a 404 response, do not return the response, continue
        # routing normally.
        def run(app, opts=OPTS)
          res = catch(:halt){super(app)}
          yield res if block_given?
          throw(:halt, res) unless opts[:not_found] == :pass && res[0] == 404
        end
      end
    end

    register_plugin(:run_handler, RunHandler)
  end
end
