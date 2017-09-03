# A group of channels, where each channel is a group of websockets
class Raze::WebSocketChannels
  INSTANCE = new
  property channels = {} of String => Raze::WebSocketChannel
end

module Raze
  def self.ws_channel(ws_channel_name)
    ws_channels = Raze::WebSocketChannels::INSTANCE.channels
    if ws_channel = ws_channels[ws_channel_name]?
      ws_channel
    else
      ws_channels[ws_channel_name] = Raze::WebSocketChannel.new ws_channel_name
    end
  end
end
