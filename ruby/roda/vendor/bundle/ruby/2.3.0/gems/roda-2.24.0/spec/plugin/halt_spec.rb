require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "halt plugin" do
  it "should still have halt return rack response as argument given it as argument" do
    app(:halt) do |r|
      r.halt [200, {}, ['foo']]
    end

    body.must_equal  "foo"
  end

  it "should consider string argument as response body" do
    app(:halt) do |r|
      r.halt "foo"
    end

    body.must_equal  "foo"
  end

  it "should consider integer argument as response status" do
    app(:halt) do |r|
      r.halt 300
    end

    status.must_equal 300 
  end

  it "should consider other single arguments similar to block bodies" do
    app(:bare) do
      plugin :halt
      plugin :json
      route do |r|
        r.halt({'a'=>1})
      end
    end

    body.must_equal  '{"a":1}'
  end

  it "should consider 2 arguments as response status and body" do
    app(:halt) do |r|
      r.halt 300, "foo"
    end

    status.must_equal 300 
    body.must_equal "foo"
  end

  it "should handle 2nd of 2 arguments similar to block bodies" do
    app(:bare) do
      plugin :halt
      plugin :json
      route do |r|
        r.halt(300, {'a'=>1})
      end
    end

    status.must_equal 300 
    body.must_equal  '{"a":1}'
  end

  it "should consider 3 arguments as response" do
    app(:halt) do |r|
      r.halt 300, {'a'=>'b'}, "foo"
    end

    status.must_equal 300 
    header('a').must_equal 'b'
    body.must_equal "foo"
  end

  it "should consider an array as a rack response" do
    app(:halt) do |r|
      r.halt [300, {'a'=>'b'}, ["foo"]]
    end

    status.must_equal 300 
    header('a').must_equal 'b'
    body.must_equal "foo"
  end

  it "should handle 3rd of 3 arguments similar to block bodies" do
    app(:bare) do
      plugin :halt
      plugin :json
      route do |r|
        r.halt(300, {'a'=>'b'}, {'a'=>1})
      end
    end

    status.must_equal 300 
    header('a').must_equal 'b'
    body.must_equal  '{"a":1}'
  end

  it "should raise an error for too many arguments" do
    app(:halt) do |r|
      r.halt 300, {'a'=>'b'}, "foo", 1
    end

    proc{req}.must_raise(Roda::RodaError)
  end

  it "should raise an error for single argument not integer, String, or Array" do
    app(:halt) do |r|
      r.halt('a'=>'b')
    end

    proc{req}.must_raise(Roda::RodaError)
  end
end
