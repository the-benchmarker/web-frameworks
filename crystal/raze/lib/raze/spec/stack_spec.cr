require "./spec_helper"

class CustomHandler1 < Raze::Handler
end

class CustomHandler2 < Raze::Handler
end

describe Raze::Stack do
  it "can take an Array of handlers and a block" do
    handler1 = CustomHandler1.new
    handler2 = CustomHandler2.new
    stack = Raze::Stack.new [handler1, handler2] do |ctx|
    end
    stack.class.should eq(Raze::Stack)
    typeof(stack.middlewares).should eq(Array(Raze::Handler))
    stack.middlewares.size.should eq(2)
    stack.block.nil?.should eq(false)
  end
  it "can take an Array of handlers with no block" do
    handler1 = CustomHandler1.new
    handler2 = CustomHandler2.new
    stack = Raze::Stack.new [handler1, handler2]
    stack.class.should eq(Raze::Stack)
    typeof(stack.middlewares).should eq(Array(Raze::Handler))
    stack.middlewares.size.should eq(2)
    stack.block.nil?.should eq(true)
  end
  it "can take a Tuple of handlers and a block" do
    handler1 = CustomHandler1.new
    handler2 = CustomHandler2.new
    stack = Raze::Stack.new handler1, handler2 do |ctx|
    end
    stack.class.should eq(Raze::Stack)
    typeof(stack.middlewares).should eq(Array(Raze::Handler))
    stack.middlewares.size.should eq(2)
    stack.block.nil?.should eq(false)
  end
  it "can take a Tuple of handlers without a block" do
    handler1 = CustomHandler1.new
    handler2 = CustomHandler2.new
    stack = Raze::Stack.new handler1, handler2
    stack.class.should eq(Raze::Stack)
    typeof(stack.middlewares).should eq(Array(Raze::Handler))
    stack.middlewares.size.should eq(2)
    stack.block.nil?.should eq(true)
  end
  it "can take a lone block" do
    stack = Raze::Stack.new do |ctx|
    end
    stack.class.should eq(Raze::Stack)
    typeof(stack.middlewares).should eq(Array(Raze::Handler))
    stack.middlewares.size.should eq(0)
    stack.block.nil?.should eq(false)
  end
end
