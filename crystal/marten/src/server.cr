require "./project"

System.cpu_count.times do |i|
  Process.fork do
    Marten.start
  end
end

sleep
