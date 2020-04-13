# frozen_string_literal: true

# Specifies the `port` that Puma will listen on to receive requests; default is 3000.
port ENV.fetch('PORT') { 3000 }

# Specifies the `environment` that Puma will run in.
environment ENV.fetch('RACK_ENV') { 'development' }

# Specifies the number of `workers` to boot in clustered mode.
# Workers are forked webserver processes. If using threads and workers together
# the concurrency of the application would be max `threads` * `workers`.
# Workers do not work on JRuby or Windows (both of which do not support
# processes).
require 'etc'
workers ENV.fetch("WEB_CONCURRENCY") { Etc.nprocessors }

# Use the `preload_app!` method when specifying a `workers` number.
# This directive tells Puma to first boot the application and load code
# before forking the application. This takes advantage of Copy On Write
# process behavior so workers use less memory. If you use this option
# you need to make sure to reconnect any threads in the `on_worker_boot`
# block.
preload_app!
