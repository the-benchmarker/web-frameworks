require "../config/*"

Amber.env = "production"

System.cpu_count.times do |i|
  Process.fork do
    Amber::Server.start
  end
end

sleep
