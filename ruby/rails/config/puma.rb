require 'concurrent-ruby'

max_threads_count = Integer(ENV.fetch('RAILS_MAX_THREADS', 3))
min_threads_count = Integer(ENV.fetch('RAILS_MIN_THREADS') { max_threads_count })
threads min_threads_count, max_threads_count

worker_count = Integer(ENV.fetch('WEB_CONCURRENCY') { Concurrent.physical_processor_count })
workers worker_count if worker_count > 1

quiet
log_requests false
custom_logger File.open(File::NULL, 'w')
stdout_redirect File::NULL, File::NULL

# Keep the listener on plain HTTP, including when CLI binds are supplied.
clear_binds!
port ENV.fetch('PORT', 3000)
environment 'production'
pidfile ENV['PIDFILE'] if ENV['PIDFILE']
