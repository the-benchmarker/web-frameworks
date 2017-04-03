# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The hooks plugin adds before and after hooks to the request cycle.
    #
    #   plugin :hooks
    #
    #   before do
    #     request.redirect('/login') unless logged_in?
    #     @time = Time.now
    #   end
    #
    #   after do |res|
    #     logger.notice("Took #{Time.now - @time} seconds")
    #   end
    #
    # Note that in general, before hooks are not needed, since you can just
    # run code at the top of the route block:
    #
    #   route do |r|
    #     r.redirect('/login') unless logged_in?
    #     # ...
    #   end
    #
    # However, this code makes it easier to write after hooks, as well as
    # handle cases where before hooks are added after the route block.
    #
    # Note that the after hook is called with the rack response array
    # of status, headers, and body.  If it wants to change the response,
    # it must mutate this argument, calling <tt>response.status=</tt> inside
    # an after block will not affect the returned status.
    module Hooks
      def self.configure(app)
        app.opts[:before_hook] ||= nil
        app.opts[:after_hook] ||= nil
      end

      module ClassMethods
        # Add an after hook.  If there is already an after hook defined,
        # use a proc that instance_execs the existing after proc and
        # then instance_execs the given after proc, so that the given
        # after proc always executes after the previous one.
        def after(&block)
          opts[:after_hook] = if b = opts[:after_hook]
            proc do |res|
              instance_exec(res, &b)
              instance_exec(res, &block)
            end
          else
            block
          end
        end

        # Add a before hook.  If there is already a before hook defined,
        # use a proc that instance_execs the give before proc and
        # then instance_execs the existing before proc, so that the given
        # before proc always executes before the previous one.
        def before(&block)
          opts[:before_hook] = if b = opts[:before_hook]
            proc do
              instance_exec(&block)
              instance_exec(&b)
            end
          else
            block
          end
        end
      end

      module InstanceMethods
        # Before routing, execute the before hooks, and
        # execute the after hooks before returning.
        def call(&block)
          res = super do |r|
            if b = opts[:before_hook]
              instance_exec(&b)
            end

            instance_exec(r, &block)
          end
        ensure
          if b = opts[:after_hook]
            instance_exec(res, &b)
          end
        end
      end
    end

    register_plugin(:hooks, Hooks)
  end
end
