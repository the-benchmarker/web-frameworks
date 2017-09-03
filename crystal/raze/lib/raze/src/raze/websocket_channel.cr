# A group of websockets
class Raze::WebSocketChannel
  property websockets = [] of HTTP::WebSocket
  property channel_name : String

  def initialize(@channel_name)
  end

  def add(sock)
    websockets << sock unless websockets.find { |ws| ws.object_id == sock.object_id }
    sock.on_close { remove sock }
    self
  end

  def remove(sock : HTTP::WebSocket)
    removed_socks = websockets.reject! { |ws| ws.object_id == sock.object_id }
    Raze::WebSocketChannels::INSTANCE.channels.delete(@channel_name) if websockets.size <= 0
  end

  def remove(sock : HTTP::WebSocket, &block : Raze::WebSocketChannel -> _)
    remove sock
    block.call(self) if websockets.size > 0
  end

  def broadcast(msg : String)
    websockets.each do |ws|
      ws.send(msg)
    end
  end

  def size
    websockets.size
  end
end
