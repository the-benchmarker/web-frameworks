class Raze::WebSocketStack
  getter middlewares
  getter block

  # A sub tree is used in case this stack is indexed using a wildcard
  # For example, if there is one stack at the path "/hel**" and another at the
  # path "/hello", the latter would be in the subtree of the first
  property tree : Radix::Tree(Raze::WebSocketStack) | Nil = nil

  def initialize(handlers : Array(Raze::WebSocketHandler), &block : HTTP::WebSocket, HTTP::Server::Context -> Void)
    @middlewares = handlers
    @block = block
  end

  def initialize(*handlers, &block : HTTP::WebSocket, HTTP::Server::Context -> Void)
    @middlewares = [] of Raze::WebSocketHandler
    handlers.each { |mw| @middlewares << mw }
    @block = block
  end

  def initialize(*handlers)
    @middlewares = [] of Raze::WebSocketHandler
    handlers.each { |mw| @middlewares << mw }
    @block = nil
  end

  def initialize(handlers : Array(Raze::WebSocketHandler))
    @middlewares = handlers
    @block = nil
  end

  def initialize(&block : HTTP::WebSocket, HTTP::Server::Context -> Void)
    @middlewares = [] of Raze::WebSocketHandler
    @block = block
  end

  def concat(stack : Raze::WebSocketStack)
    @middlewares.concat stack.middlewares
    @block = stack.block
  end

  def block?
    true unless @block.nil?
  end

  # def initialize(middlewares : Array(Raze::WebSocketHandler))
  #   @middlewares = middlewares
  #   @block = nil
  # end

  def run(ws : HTTP::WebSocket, ctx : HTTP::Server::Context)
    self.next(0, ws, ctx)
  end

  def next(index, ws : HTTP::WebSocket, ctx : HTTP::Server::Context)
    if mw = @middlewares[index]?
      mw.call ws, ctx, ->{ self.next(index + 1, ws, ctx) }
    elsif block = @block
      block.call(ws, ctx)
    elsif _tree = tree
      # find and run the sub tree
      find_result = _tree.find(radix_path(ctx.request.path))
      if find_result.found?
        ctx.params = find_result.params
        find_result.payload.as(Raze::WebSocketStack).run(ws, ctx)
      end
    end
  end

  private def radix_path(path)
    String.build do |str|
      str << "/WS"
      str << path
    end
  end
end
