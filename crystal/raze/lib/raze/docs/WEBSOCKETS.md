# Websockets

```ruby
Raze.ws "/yee/boi" do |sock, ctx|
  sock.send("connected")

  # Add this socket to a channel for easy "broadcast" to a group of sockets.
  # Adding to a channel called "yeezer". A channel it is created if it doesn't exist.
  Raze.ws_channel("yeezer").add sock

  # Create a user id for this websocket connection
  user_id = "user:#{Raze.ws_channel("yeezer").size}"

  # Optional: This will print how many sockets are connected to each channel
  Raze::WebSocketChannels::INSTANCE.channels.each do|chan_name, chan|
    puts "#{chan_name} has #{chan.size} connections"
  end

  sock.on_message do |msg|
    # broadcast a json message to each websocket in the channel
    Raze.ws_channel("yeezer").broadcast({"user_id" => user_id, "msg" => msg}.to_json)
  end

  sock.on_close do
    # remove the socket from the channel, and broadcast the user has left
    Raze.ws_channel("yeezer").remove sock do |channel|
      channel.broadcast({"user_id" => user_id, "msg" => "user disconnected"}.to_json)
    end

    # Optional: print how many sockets are connected to each channel
    Raze::WebSocketChannels::INSTANCE.channels.each do|chan_name, chan|
      puts "#{chan_name} has #{chan.size} connections"
    end
  end
end
```
