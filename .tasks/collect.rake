require 'open3'
require 'csv'
require 'etc'
require 'bigdecimal/util'

task :collect do
  duration = ENV.fetch('DURATION', 10)
  language = ENV.fetch('LANGUAGE') { raise 'please provide the language' }
  framework = ENV.fetch('FRAMEWORK') { raise 'please provide the target framework' }
  concurrencies = ENV.fetch('CONCURRENCIES', '10')
  routes = ENV.fetch('ROUTES', 'GET:/')
  database = ENV.fetch('DATABASE_URL') { raise 'please provide a DATABASE_URL (pg only)' }
  hostname = ENV.fetch('HOSTNAME')
  engine = ENV.fetch('ENGINE')

  `oha --disable-keepalive -c 20 -z 5sec -t 8sec -j --no-tui http://#{hostname}:3000`

  routes.split(',').each do |route|
    method, uri = route.split(':')
    concurrencies.split(',').each do |concurrency|
      command = format(
        "oha --disable-keepalive -c %<concurrency>s -z %<duration>ssec -t 8sec -j --no-tui http://%<hostname>s:3000#{uri}", concurrency:, duration:, hostname:
      )
      out = File.open("#{language}/#{framework}/#{concurrency}_#{uri.gsub('/','_')}.json",'w')
      Open3.popen3(command) do |_, stdout, stderr|
        out.write(stdout.read)
        $stderr.puts "#{stderr.read} for #{route}"
      end
      out.close()
    end
  end
end
