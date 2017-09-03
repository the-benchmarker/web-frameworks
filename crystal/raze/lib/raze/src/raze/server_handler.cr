require "./ext/context"

# The main server handler.
class Raze::ServerHandler
  include HTTP::Handler
  INSTANCE = new

  # the radix tree of defined paths and the associated stacks
  getter tree

  def initialize
    @tree = Radix::Tree(Raze::Stack).new
    @radix_paths = [] of String
  end

  def clear_tree
    @tree = Radix::Tree(Raze::Stack).new
    @radix_paths = [] of String
  end

  # Adds a stack to the tree
  #
  # NOTE: This method is only ran at server startup so the extra internal sanity
  # checks will not affect server performance
  def add_stack(method, path, stack)
    node = radix_path(method, path)
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

    lookup_result = @tree.find node

    if lookup_result.found?
      # check if stack has an ending block
      existing_stack = lookup_result.payload.as(Raze::Stack)
      raise "There is already an existing block for #{method.upcase} #{path}." if existing_stack.block?

      # if same key (ie. method and path) then combine stacks
      if lookup_result.key == node
        existing_stack.concat stack
        # else, there is some globbing, so append stack to to existing stack's tree
      else
        existing_stack.add_sub_tree stack, node, method, path
      end
    else
      add_to_tree(node, stack)
      add_to_tree(radix_path("HEAD", path), Raze::Stack.new() { |ctx| "" }) if method == "GET"
    end
  end

  # TODO: allow passing a block to call
  # def call(context, &block)
  # end

  def call(ctx)
    handle_request ctx
  end

  private def handle_request(ctx)
    # check if there is a stack in radix that matches path
    node = radix_path ctx.request.method, ctx.request.path
    lookup_result = @tree.find node
    raise Raze::Exceptions::RouteNotFound.new(ctx) unless lookup_result.found?

    # set context params
    ctx.params = lookup_result.params

    # run the stack
    stack = lookup_result.payload.as(Raze::Stack)
    content = stack.run ctx
  ensure
    if Raze.config.error_handlers.has_key?(ctx.response.status_code)
      raise Raze::Exceptions::CustomException.new(ctx)
    elsif !ctx.response.closed? && content.is_a?(String)
      ctx.response.print content
    end
  end

  private def add_to_tree(node : String, stack : Raze::Stack)
    @tree.add node, stack
    @radix_paths << node
  end

  private def radix_path(method, path)
    String.build do |str|
      str << "/"
      str << method.downcase
      str << path
    end
  end
end
