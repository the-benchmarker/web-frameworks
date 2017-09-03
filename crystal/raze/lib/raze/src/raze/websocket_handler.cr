class Raze::WebSocketHandler
  def call(ws, context, done)
    done.call
  end
end
