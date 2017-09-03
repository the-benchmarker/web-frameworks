require "./spec_helper"

class SetStatus202 < Raze::Handler
  def call(ctx, done)
    ctx.response.status_code = 202
    done.call
  end
end

class HelloWorld < Raze::Handler
  def call(ctx, done)
    ctx.response.print "hello, world!!!"
    done.call
  end
end


describe Raze::ServerHandler do
  it "can take a request" do
    stack = Raze::Stack.new do |ctx|
      "hello, world!"
    end
    stack2 = Raze::Stack.new do |ctx|
      "hello, #{ctx.params["name"]}!"
    end

    Raze::ServerHandler::INSTANCE.add_stack "GET", "/hello", stack
    Raze::ServerHandler::INSTANCE.add_stack "GET", "/hello/:name", stack2

    request = HTTP::Request.new("GET", "/hello")
    client_response = call_request_on_app(request)
    client_response.body.should eq("hello, world!")

    request = HTTP::Request.new("GET", "/hello/sam")
    client_response = call_request_on_app(request)
    client_response.body.should eq("hello, sam!")
    client_response.status_code.should eq(200)
  end

  it "can run a stack of middlewares" do
    stack = Raze::Stack.new SetStatus202.new() do |ctx|
      "hello, world!"
    end
    Raze::ServerHandler::INSTANCE.add_stack "GET", "/", stack
    request = HTTP::Request.new("GET", "/")
    client_response = call_request_on_app(request)
    client_response.body.should eq("hello, world!")
    client_response.status_code.should eq(202)
  end

  it "can run a stack of middlewares without a block" do
    stack = Raze::Stack.new SetStatus202.new(), HelloWorld.new()
    Raze::ServerHandler::INSTANCE.add_stack "GET", "/", stack
    request = HTTP::Request.new("GET", "/")
    client_response = call_request_on_app(request)
    client_response.body.should eq("hello, world!!!")
    client_response.status_code.should eq(202)
  end

  it "should successfully sub-tree the stacks" do
    stack1 = Raze::Stack.new SetStatus202.new()
    stack2 = Raze::Stack.new HelloWorld.new()
    stack3 = Raze::Stack.new do |ctx|
      "yee"
    end

    Raze::ServerHandler::INSTANCE.add_stack "GET", "/hel*", stack1
    Raze::ServerHandler::INSTANCE.add_stack "GET", "/hello*", stack2
    Raze::ServerHandler::INSTANCE.add_stack "GET", "/hell", stack3
    Raze::ServerHandler::INSTANCE.add_stack "GET", "/yeezy", stack3

    request = HTTP::Request.new("GET", "/hell")
    client_response = call_request_on_app(request)
    client_response.body.should eq("yee")
    client_response.status_code.should eq(202)

    request = HTTP::Request.new("GET", "/helloworld")
    client_response = call_request_on_app(request)
    client_response.body.should eq("hello, world!!!")
    client_response.status_code.should eq(202)

    request = HTTP::Request.new("GET", "/yeezy")
    client_response = call_request_on_app(request)
    client_response.body.should eq("yee")
    client_response.status_code.should eq(200)
  end
end
