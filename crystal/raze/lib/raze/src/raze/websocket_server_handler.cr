class Raze::WebSocketServerHandler < HTTP::WebSocketHandler
  INSTANCE = new do |sock, ctx|
    lookup_result = Raze::WebSocketServerHandler::INSTANCE.lookup_route(ctx.request.path)
    ctx.params = lookup_result.params
    # ctx.query = ctx.request.query

    stack = lookup_result.payload.as(Raze::WebSocketStack)
    content = stack.run(sock, ctx)
  end

  @radix_paths = [] of String

  def initialize(&@proc : HTTP::WebSocket, HTTP::Server::Context -> Void)
    @tree = Radix::Tree(Raze::WebSocketStack).new
  end

  def add_stack(path, stack)
    node = radix_path(path)
    @radix_paths.each do |existing_path|
      # if you can:
      #   (1) add the new radix path to a tree by itself, and
      #   (2) look up an existing path and it matches the new radix path
      # then less specific path (e.g. with a "*" or ":") is being defined after a more specific path
      temp_tree = Radix::Tree(String).new
      temp_tree.add node, node
      temp_result = temp_tree.find existing_path
      if temp_result.found? && temp_result.payload == node
        raise "the less specific path \"#{node}\" must be defined before the more specific path \"#{existing_path}\""
      end
    end

    lookup_result = lookup_route(path)
    if lookup_result.found?
      # check if stack has an ending block
      existing_stack = lookup_result.payload.as(Raze::WebSocketStack)
      raise "There is already an existing block for WS #{path}" if existing_stack.block?
      if lookup_result.key == node
        existing_stack.concat stack
      else
        # add tree to the existing stack because there is some globbing going on
        existing_stack.tree = Radix::Tree(Raze::WebSocketStack).new unless existing_stack.tree
        sub_tree = existing_stack.tree.as(Radix::Tree(Raze::WebSocketStack))
        # add stack to this sub tree
        sub_tree.add node, stack
      end
    else
      @tree.add node, stack
      @radix_paths << node
    end
  end

  def lookup_route(path)
    @tree.find radix_path(path)
  end

  private def radix_path(path)
    String.build do |str|
      str << "/ws"
      str << path
    end
  end

  def call(ctx)
    super
  ensure
    if Raze.config.error_handlers.has_key?(ctx.response.status_code)
      raise Raze::Exceptions::CustomException.new(ctx)
    end
  end

  # def initialize(@path : String, &@proc : HTTP::WebSocket, HTTP::Server::Context -> Void)
  #   Raze.config.global_handlers << self
  # end

  # def call(context)
  #   return call_next(context) unless context.request.path.not_nil! == @path
  #   super
  # end
end
