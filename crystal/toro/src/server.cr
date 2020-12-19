require "toro"

class App < Toro::Router
  def routes
    get do
      context.response.print ""
    end

    on "user" do
      post do
        context.response.print ""
      end

      on :id do
        get do
          context.response.print inbox[:id]
        end
      end
    end
  end
end

System.cpu_count.times do |i|
  Process.fork do
    App.run 3000 do |server|
      server.bind_tcp("0.0.0.0", 3000, true)
      server.listen
    end
  end
end

sleep
