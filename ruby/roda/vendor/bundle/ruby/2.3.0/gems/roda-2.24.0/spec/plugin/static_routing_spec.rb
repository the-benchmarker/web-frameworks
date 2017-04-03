require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "static_routing plugin" do 
  it "adds support for static routes that are taken before normal routes" do
    app(:bare) do
      plugin :static_routing
      static_route "/foo" do |r|
        "#{r.path}:#{r.remaining_path}"
      end
      static_route "/bar" do |r|
        r.get{"GET:#{r.path}:#{r.remaining_path}"}
        r.post{"POST:#{r.path}:#{r.remaining_path}"}
      end
      static_get "/bar" do |r|
        r.get{"GET2:#{r.path}:#{r.remaining_path}"}
      end
      static_route "/quux" do |r|
         r.halt [500, {}, []]
      end

      route do |r|
        r.on 'foo' do
          r.get true do
            'foo1'
          end
          r.root do
            'foo2'
          end
        end

        r.get 'baz' do
          'baz'
        end
      end
    end

    2.times do
      body('/foo').must_equal '/foo:'
      body('/foo/').must_equal 'foo2'
      body('/bar').must_equal 'GET2:/bar:'
      body('/bar', 'REQUEST_METHOD'=>'POST').must_equal 'POST:/bar:'
      status('/bar', 'REQUEST_METHOD'=>'PATCH').must_equal 404
      body('/baz').must_equal 'baz'
      status('/quux').must_equal 500
      @app = Class.new(@app)
    end
  end

  it "works with hooks plugin if loaded after" do
    a = []
    app(:bare) do
      plugin :hooks
      plugin :static_routing

      before{a << 1}
      after{a << 2}

      static_route "/foo" do |r|
        a << 3
        "bar"
      end

      route{}
    end
    body('/foo').must_equal 'bar'
    a.must_equal [1,3,2]
  end

  it "does not allow placeholders in static routes" do
    app(:bare) do
      plugin :static_routing
      static_route "/:foo" do |r|
        "#{r.path}:#{r.remaining_path}"
      end

      route{}
    end
    body('/:foo').must_equal '/:foo:'
    status('/a').must_equal 404
  end

  it "duplicates data structures in subclasses" do
    app(:bare) do
      plugin :static_routing
      static_route "/foo" do |r|
        'foo'
      end

      route{}
    end

    old_app = @app
    @app = Class.new(old_app)
    old_app.static_route '/bar' do |r|
      'bar1'
    end
    old_app.static_get '/foo' do |r|
      'foop'
    end
    @app.static_route '/bar' do |r|
      'bar2'
    end

    body('/foo').must_equal 'foo'
    body('/bar').must_equal 'bar2'
    body('/foo', 'REQUEST_METHOD'=>'POST').must_equal 'foo'
    @app = old_app
    body('/foo').must_equal 'foop'
    body('/bar').must_equal 'bar1'
    body('/foo', 'REQUEST_METHOD'=>'POST').must_equal 'foo'
  end

  it "freezes static routes when app is frozen" do
    app(:bare) do
      plugin :static_routing
      static_route "/foo"
      freeze

      proc do
        static_get "/foo"
      end.must_raise

      proc do
        static_route "/bar"
      end.must_raise
    end
  end
end
