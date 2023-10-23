require "../config/*"

Amber.env = "production"

if ARGV.size > 0 && ARGV[0] == "--start-amber"
  Amber::Server.start
else
  System.cpu_count.times do |i|
    Process.new("/usr/bin/app", ["--start-amber"])
  end
end

sleep
