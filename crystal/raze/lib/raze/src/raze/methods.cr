HTTP_METHODS_OPTIONS = %w(get post put patch delete options)

{% for method in HTTP_METHODS_OPTIONS %}

  # ```
  # Raze.get "/hello" do |context|
  #   "Hello, world!"
  # end
  # ```

  def {{method.id}}(path, &block : HTTP::Server::Context -> (HTTP::Server::Context|String|Int32|Int64|Bool|Nil))
    raise "websocket path \"#{path}\" must start with a \"/\"" unless path.starts_with? "/"
    stack = Raze::Stack.new(&block)
    Raze::ServerHandler::INSTANCE.add_stack {{method}}.upcase, path, stack
  end

  # ```
  # Raze.get "/hello", [CustomHandler.new, OtherHandler.new]
  # ```

  def {{method.id}}(path, middlewares : Array(Raze::Handler))
    raise "websocket path \"#{path}\" must start with a \"/\"" unless path.starts_with? "/"
    stack = Raze::Stack.new(middlewares)
    Raze::ServerHandler::INSTANCE.add_stack {{method}}.upcase, path, stack
  end

  # ```
  # Raze.get "/hello", [CustomHandler.new, OtherHandler.new] do |context|
  #   "Hello, world!"
  # end
  # ```

  def {{method.id}}(path, middlewares : Array(Raze::Handler), &block : HTTP::Server::Context -> (HTTP::Server::Context|String|Int32|Int64|Bool|Nil))
    raise "websocket path \"#{path}\" must start with a \"/\"" unless path.starts_with? "/"
    stack = Raze::Stack.new(middlewares, &block)
    Raze::ServerHandler::INSTANCE.add_stack {{method}}.upcase, path, stack
  end

  # ```
  # Raze.get "/hello", CustomHandler.new, OtherHandler.new
  # ```

  def {{method.id}}(path, *middlewares)
    raise "websocket path \"#{path}\" must start with a \"/\"" unless path.starts_with? "/"
    stack = Raze::Stack.new(*middlewares)
    Raze::ServerHandler::INSTANCE.add_stack {{method}}.upcase, path, stack
  end

  # ```
  # Raze.get "/hello", CustomHandler.new, OtherHandler.new do |context|
  #   "Hello, world!"
  # end
  # ```

  def {{method.id}}(path, *middlewares, &block : HTTP::Server::Context -> (HTTP::Server::Context|String|Int32|Int64|Bool|Nil))
    raise "path \"#{path}\" must start with a \"/\"" unless path.starts_with? "/"
    stack = Raze::Stack.new(*middlewares, &block)
    Raze::ServerHandler::INSTANCE.add_stack {{method}}.upcase, path, stack
  end

{% end %}

def ws(path, &block : HTTP::WebSocket, HTTP::Server::Context -> Void)
  raise "websocket path \"#{path}\" must start with a \"/\"" unless path.starts_with? "/"
  stack = Raze::WebSocketStack.new(&block)
  Raze::WebSocketServerHandler::INSTANCE.add_stack path, stack
end

def ws(path, middlewares : Array(Raze::WebSocketHandler))
  raise "websocket path \"#{path}\" must start with a \"/\"" unless path.starts_with? "/"
  stack = Raze::WebSocketStack.new(middlewares)
  Raze::WebSocketServerHandler::INSTANCE.add_stack path, stack
end

def ws(path, middlewares : Array(Raze::WebSocketHandler), &block : HTTP::WebSocket, HTTP::Server::Context -> Void)
  raise "websocket path \"#{path}\" must start with a \"/\"" unless path.starts_with? "/"
  stack = Raze::WebSocketStack.new(middlewares, &block)
  Raze::WebSocketServerHandler::INSTANCE.add_stack path, stack
end

def ws(path, *middlewares)
  raise "websocket path \"#{path}\" must start with a \"/\"" unless path.starts_with? "/"
  stack = Raze::WebSocketStack.new(*middlewares)
  Raze::WebSocketServerHandler::INSTANCE.add_stack path, stack
end

def ws(path, *middlewares, &block : HTTP::WebSocket, HTTP::Server::Context -> Void)
  raise "websocket path \"#{path}\" must start with a \"/\"" unless path.starts_with? "/"
  stack = Raze::WebSocketStack.new(*middlewares, &block)
  Raze::WebSocketServerHandler::INSTANCE.add_stack path, stack
end

def error(status_code, &block : HTTP::Server::Context, Exception -> _)
  Raze.config.error_handlers[status_code] = ->(context : HTTP::Server::Context, error : Exception) { block.call(context, error).to_s }
end
