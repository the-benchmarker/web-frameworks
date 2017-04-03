# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The per_thread_caching plugin changes the default cache
    # from being a shared thread safe cache to a separate cache per
    # thread.  This means getting or setting values no longer
    # needs a mutex, which may be faster when using a thread pool.
    # However, since the caches are no longer shared, this will
    # take up more memory.
    #
    # Using this plugin changes the matcher regexp cache to use
    # per-thread caches, and changes the default for future
    # thread-safe caches to use per-thread caches.
    #
    # If you want the render plugin's template cache to use
    # per-thread caches, you should load this plugin before the
    # render plugin.
    module PerThreadCaching
      def self.configure(app)
        app::RodaRequest.match_pattern_cache = app.thread_safe_cache
      end

      class Cache
        # Mutex used to ensure multiple per-thread caches
        # don't use the same key
        MUTEX = ::Mutex.new

        n = 0
        # Auto incrementing number proc used to make sure
        # multiple thread-thread caches don't use the same key.
        N = lambda{MUTEX.synchronize{n += 1}}

        # Store unique symbol used to look up in the per
        # thread caches.
        def initialize
          @o = :"roda_per_thread_cache_#{N.call}"
        end

        # Return the current thread's cached value.
        def [](key)
          _hash[key]
        end

        # Set the current thread's cached value.
        def []=(key, value)
          _hash[key] = value
        end

        private

        # The current thread's cache.
        def _hash
          ::Thread.current[@o] ||= {}
        end
      end

      module ClassMethods
        # Use the per-thread cache instead of the default cache.
        def thread_safe_cache
          Cache.new
        end
      end
    end

    register_plugin(:per_thread_caching, PerThreadCaching)
  end
end
