# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The run_append_slash plugin makes +r.run+ use +/+ as the +PATH_INFO+
    # when calling the rack application if +PATH_INFO+ would be empty.
    # Example:
    #
    #   route do |r|
    #     r.on "a" do
    #       r.run App
    #     end
    #   end
    #
    #   # without run_append_slash: 
    #   # GET /a => App gets "" as PATH_INFO
    #   # GET /a/ => App gets "/" as PATH_INFO
    #
    #   # with run_append_slash: 
    #   # GET /a => App gets "/" as PATH_INFO
    #   # GET /a/ => App gets "/" as PATH_INFO
    module RunAppendSlash
      OPTS = {}.freeze
      # Set plugin specific options.  Options:
      # :use_redirects :: Whether to issue 302 redirects when appending the
      #                   trailing slash.
      def self.configure(app, opts=OPTS)
        app.opts[:run_append_slash_redirect] = !!opts[:use_redirects]
      end

      module RequestMethods
        # Calls the given rack app. If the path matches the root of the app but
        # does not contain a trailing slash, a trailing slash is appended to the
        # path internally, or a redirect is issued when configured with
        # <tt>use_redirects: true</tt>.
        def run(app)
          if remaining_path.empty?
            if scope.opts[:run_append_slash_redirect]
              redirect("#{path}/")
            else
              @remaining_path += '/'
            end
          end
          super
        end
      end
    end

    register_plugin(:run_append_slash, RunAppendSlash)
  end
end
