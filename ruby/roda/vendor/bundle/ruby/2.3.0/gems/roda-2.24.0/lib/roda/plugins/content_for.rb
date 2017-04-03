# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The content_for plugin is designed to be used with the
    # render plugin, allowing you to store content inside one
    # template, and retrieve that content inside a separate
    # template.  Most commonly, this is so view templates
    # can set content for the layout template to display outside
    # of the normal content pane.
    #
    # In the template in which you want to store content, call
    # content_for with a block:
    #
    #   <% content_for :foo do %>
    #     Some content here.
    #   <% end %>
    #
    # You can also set the raw content as the second argument,
    # instead of passing a block:
    #
    #   <% content_for :foo, "Some content" %>
    #
    # In the template in which you want to retrieve content,
    # call content_for without the block:
    #
    #   <%= content_for :foo %>
    #
    # If content_for is used multiple times with the same key,
    # by default, the last call will override previous calls.
    # If you want to append to the content, pass the :append
    # option when loading the plugin:
    #
    #   plugin :content_for, :append=>true
    module ContentFor
      # Depend on the render plugin, since this plugin only makes
      # sense when the render plugin is used.
      def self.load_dependencies(app, _opts = {})
        app.plugin :render
      end

      # Configure whether to append or overwrite if content_for
      # is called multiple times to set data. Overwrite is default, use
      # the :append option to append.
      def self.configure(app, opts = {})
        app.opts[:append_content_for] = opts.fetch(:append, false)
      end

      module InstanceMethods
        # If called with a block, store content enclosed by block
        # under the given key.  If called without a block, retrieve
        # stored content with the given key, or return nil if there
        # is no content stored with that key.
        def content_for(key, value=nil, &block)
          append = opts[:append_content_for]

          if block || value
            if block
              outvar = render_opts[:template_opts][:outvar]
              buf_was = instance_variable_get(outvar)

              # Use temporary output buffer for ERB-based rendering systems
              instance_variable_set(outvar, String.new)
              value = Tilt[render_opts[:engine]].new(&block).render
              instance_variable_set(outvar, buf_was)
            end

            @_content_for ||= {}

            if append
              (@_content_for[key] ||= []) << value
            else
              @_content_for[key] = value
            end
          elsif @_content_for && (value = @_content_for[key])
            if append
              value = value.join
            end

            value
          end
        end
      end
    end

    register_plugin(:content_for, ContentFor)
  end
end
