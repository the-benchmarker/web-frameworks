# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The view_options plugin allows you to override view and layout
    # options and locals for specific branches and routes.
    #
    #   plugin :render
    #   plugin :view_options
    #
    #   route do |r|
    #     r.on "users" do
    #       set_layout_options :template=>'users_layout'
    #       set_layout_locals :title=>'Users'
    #       set_view_options :engine=>'haml'
    #       set_view_locals :footer=>'(c) Roda'
    #
    #       # ...
    #     end
    #   end
    #
    # The options and locals you specify have higher precedence than
    # the render plugin options, but lower precedence than options
    # you directly pass to the view/render methods.
    #
    # = View Subdirectories
    #
    # The view_options plugin also has special support for sites
    # that have outgrown a flat view directory and use subdirectories
    # for views.  It allows you to set the view directory to
    # use, and template names that do not contain a slash will
    # automatically use that view subdirectory.  Example:
    #
    #   plugin :render, :layout=>'./layout'
    #   plugin :view_options
    #
    #   route do |r|
    #     r.on "users" do
    #       set_view_subdir 'users'
    #       
    #       r.get :id do
    #         append_view_subdir 'profile'
    #         view 'index' # uses ./views/users/profile/index.erb
    #       end
    #
    #       r.get 'list' do
    #         view 'lists/users' # uses ./views/lists/users.erb
    #       end
    #     end
    #   end
    #
    # Note that when a view subdirectory is set, the layout will
    # also be looked up in the subdirectory unless it contains
    # a slash.  So if you want to use a view subdirectory for
    # templates but have a shared layout, you should make sure your
    # layout contains a slash, similar to the example above.
    #
    # = Per-branch HTML escaping
    #
    # If you have an existing Roda application that doesn't use
    # automatic HTML escaping for <tt><%= %></tt> tags via the
    # :render plugin's :escape option, but you want to switch to
    # using the :escape option, you can now do so without making
    # all changes at once.  With set_view_options, you can now
    # specify escaping or not on a per branch basis in the routing
    # tree:
    #
    #   plugin :render, :escape=>true
    #   plugin :view_options
    #
    #   route do |r|
    #     # Don't escape <%= %> by default
    #     set_view_options :template_opts=>{:engine_class=>nil}
    #
    #     r.on "users" do
    #       # Escape <%= %> in this branch
    #       set_view_options :template_opts=>{:engine_class=>render_opts[:template_opts][:engine_class]}
    #     end
    #   end
    module ViewOptions
      # Load the render plugin before this plugin, since this plugin
      # works by overriding methods in the render plugin.
      def self.load_dependencies(app)
        app.plugin :render
      end

      # The following methods are created via metaprogramming:
      # set_layout_locals :: Set locals to use in the layout
      # set_layout_options :: Set options to use when rendering the layout
      # set_view_locals :: Set locals to use in the view
      # set_view_options :: Set options to use when rendering the view
      module InstanceMethods
        %w'layout view'.each do |type|
          %w'locals options'.each do |var|
            v = "_#{type}_#{var}"
            module_eval(<<-END, __FILE__, __LINE__+1)
              def set#{v}(opts)
                if @#{v}
                  @#{v} = Hash[@#{v}].merge!(opts)
                else
                  @#{v} = opts
                end
              end
            END
          end
        end

        # Append a view subdirectory to use.  If there hasn't already
        # been a view subdirectory set, this just sets it to the argument.
        # If there has already been a view subdirectory set, this sets
        # the view subdirectory to a subdirectory of the existing
        # view subdirectory.
        def append_view_subdir(v)
          if subdir = @_view_subdir
            set_view_subdir("#{subdir}/#{v}")
          else
            set_view_subdir(v)
          end
        end

        # Set the view subdirectory to use.  This can be set to nil
        # to not use a view subdirectory.
        def set_view_subdir(v)
          @_view_subdir = v
        end

        private

        # If view options or locals have been set and this
        # template isn't a layout template, merge the options
        # and locals into the returned hash.
        def parse_template_opts(template, opts)
          t_opts = super

          unless t_opts[:_is_layout]
            if v_opts = @_view_options
              t_opts.merge!(v_opts)
            end

            if v_locals = @_view_locals
              t_opts[:locals] = if t_locals = t_opts[:locals]
                Hash[v_locals].merge!(t_locals)
              else
                v_locals
              end
            end
          end

          t_opts
        end

        # If layout options or locals have been set,
        # merge the options and locals into the returned hash.
        def render_layout_opts
          opts = super

          if l_opts = @_layout_options
            opts.merge!(l_opts)
          end

          if l_locals = @_layout_locals
            opts[:locals] = if o_locals = opts[:locals]
              Hash[o_locals].merge!(l_locals)
            else
              l_locals
            end
          end

          opts
        end

        # Override the template name to use the view subdirectory if the
        # there is a view subdirectory and the template name does not
        # contain a slash.
        def template_name(opts)
          name = super
          if (v = @_view_subdir) && name !~ /\//
            "#{v}/#{name}"
          else
            name
          end
        end
      end
    end

    register_plugin(:view_options, ViewOptions)
  end
end
