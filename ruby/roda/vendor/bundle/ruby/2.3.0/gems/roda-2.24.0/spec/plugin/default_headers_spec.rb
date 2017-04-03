require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "default_headers plugin" do 
  it "sets the default headers to use for the response" do
    h = {'Content-Type'=>'text/json', 'Foo'=>'bar'}

    app(:bare) do
      plugin :default_headers, h
      route do |r|
        r.halt response.finish_with_body([])
      end
    end

    req[1].must_equal h
    req[1].wont_be_same_as h 
  end

  it "should not override existing default headers" do
    h = {'Content-Type'=>'text/json', 'Foo'=>'bar'}

    app(:bare) do
      plugin :default_headers, h
      plugin :default_headers

      route do |r|
        r.halt response.finish_with_body([])
      end
    end

    req[1].must_equal h
  end

  it "should allow modifying the default headers by reloading the plugin" do
    app(:bare) do
      plugin :default_headers, 'Content-Type' => 'text/json'
      plugin :default_headers, 'Foo' => 'baz'

      route do |r|
        r.halt response.finish_with_body([])
      end
    end

    req[1].must_equal('Content-Type'=>'text/json', 'Foo'=>'baz')
  end

  it "should have a default Content-Type header" do
    h = {'Foo'=>'bar'}

    app(:bare) do
      plugin :default_headers, h

      route do |r|
        r.halt response.finish_with_body([])
      end
    end

    req[1].must_equal('Content-Type'=>'text/html', 'Foo'=>'bar')
  end

  it "should work correctly in subclasses" do
    h = {'Content-Type'=>'text/json', 'Foo'=>'bar'}

    app(:bare) do
      plugin :default_headers, h

      route do |r|
        r.halt response.finish_with_body([])
      end
    end

    @app = Class.new(@app)

    req[1].must_equal h
  end
end
