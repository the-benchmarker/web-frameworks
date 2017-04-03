# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The not_allowed plugin makes Roda attempt to automatically
    # support the 405 Method Not Allowed response status. The plugin
    # changes the +r.get+ and +r.post+ verb methods to automatically
    # return a 405 status if they are called with any arguments, and
    # the arguments match but the request method does not match. So
    # this code:
    #
    #   r.get '' do
    #     "a"
    #   end
    #
    # will return a 200 response for <tt>GET /</tt> and a 405
    # response for <tt>POST /</tt>.
    #
    # This plugin also changes the +r.is+ method so that if you use
    # a verb method inside +r.is+, it returns a 405 status if none
    # of the verb methods match.  So this code:
    #
    #   r.is '' do
    #     r.get do
    #       "a"
    #     end
    #
    #     r.post do
    #       "b"
    #     end
    #   end
    #
    # will return a 200 response for <tt>GET /</tt> and <tt>POST /</tt>,
    # but a 405 response for <tt>PUT /</tt>.
    #
    # Note that this plugin will probably not do what you want for
    # code such as:
    #
    #   r.get '' do
    #     "a"
    #   end
    #
    #   r.post '' do
    #     "b"
    #   end
    #
    # Since for a <tt>POST /</tt> request, when +r.get+ method matches
    # the path but not the request method, it will return an immediate
    # 405 response.  You must DRY up this code for it work correctly,
    # like this:
    #
    #   r.is '' do
    #     r.get do
    #       "a"
    #     end
    #
    #     r.post do
    #       "b"
    #     end
    #   end
    #
    # In all cases where it uses a 405 response, it also sets the +Allow+
    # header in the response to contain the request methods supported.
    #
    # This plugin depends on the all_verbs plugin.
    module NotAllowed
      # Depend on the all_verbs plugin, as this plugin overrides methods
      # defined by it and calls super.
      def self.load_dependencies(app)
        app.plugin :all_verbs
      end

      module RequestMethods
        # Keep track of verb calls inside the block.  If there are any
        # verb calls inside the block, but the block returned, assume
        # that the verb calls inside the block did not match, and
        # since there was already a successful terminal match, the
        # request method must not be allowed, so return a 405
        # response in that case.
        def is(*args)
          super(*args) do
            begin
              is_verbs = @_is_verbs = []

              ret = if args.empty?
                yield
              else
                yield(*captures)
              end

              unless is_verbs.empty?
                method_not_allowed(is_verbs.join(', '))
              end

              ret
            ensure
              @_is_verbs = nil
            end
          end
        end

        # Setup methods for all verbs.  If inside an is block and not given
        # arguments, record the verb used.  If given an argument, add an is
        # check with the arguments.
        %w'get post delete head options link patch put trace unlink'.each do |verb|
          if ::Rack::Request.method_defined?("#{verb}?")
            class_eval(<<-END, __FILE__, __LINE__+1)
              def #{verb}(*args, &block)
                if (empty = args.empty?) && @_is_verbs
                  @_is_verbs << "#{verb.to_s.upcase}"
                end
                super
                unless empty
                  is(*args){method_not_allowed("#{verb.to_s.upcase}")}
                end
              end
            END
          end
        end

        private

        # Set the response status to 405 (Method Not Allowed), and set the Allow header
        # to the given string of allowed request methods.
        def method_not_allowed(verbs)
          res = response
          res.status = 405
          res['Allow'] = verbs
        end
      end
    end

    register_plugin(:not_allowed, NotAllowed)
  end
end
