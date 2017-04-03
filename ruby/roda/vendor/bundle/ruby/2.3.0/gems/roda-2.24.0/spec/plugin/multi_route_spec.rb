require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "multi_route plugin" do 
  before do
    app(:bare) do
      plugin :multi_route

      route("get") do |r|
        r.is "" do
          "get"
        end
        
        r.is "a" do
          "geta"
        end

        "getd"
      end

      route("post") do |r|
        r.is "" do
          "post"
        end
        
        r.is "a" do
          "posta"
        end

        "postd"
      end

      route(:p) do |r|
        r.is do
          'p'
        end
      end

      route do |r|
        r.on 'foo' do
          r.multi_route do
            "foo"
          end

          r.on "p" do
            r.route(:p)
          end
        end

        r.get do
          r.route("get")

          r.is "b" do
            "getb"
          end
        end

        r.post do
          r.route("post")

          r.is "b" do
            "postb"
          end
        end
      end
    end
  end

  it "adds named routing support" do
    body.must_equal 'get'
    body('REQUEST_METHOD'=>'POST').must_equal 'post'
    body('/a').must_equal 'geta'
    body('/a', 'REQUEST_METHOD'=>'POST').must_equal 'posta'
    body('/b').must_equal 'getb'
    body('/b', 'REQUEST_METHOD'=>'POST').must_equal 'postb'
    status('/c').must_equal 404
    status('/c', 'REQUEST_METHOD'=>'POST').must_equal 404
  end

  it "works when freezing the app" do
    app.freeze
    body.must_equal 'get'
    body('REQUEST_METHOD'=>'POST').must_equal 'post'
    body('/a').must_equal 'geta'
    body('/a', 'REQUEST_METHOD'=>'POST').must_equal 'posta'
    body('/b').must_equal 'getb'
    body('/b', 'REQUEST_METHOD'=>'POST').must_equal 'postb'
    status('/c').must_equal 404
    status('/c', 'REQUEST_METHOD'=>'POST').must_equal 404

    proc{app.route("foo"){}}.must_raise
  end

  it "uses multi_route to dispatch to any named route" do
    status('/foo').must_equal 404
    body('/foo/get/').must_equal 'get'
    body('/foo/get/a').must_equal 'geta'
    body('/foo/post/').must_equal 'post'
    body('/foo/post/a').must_equal 'posta'
    body('/foo/post/b').must_equal 'foo'
  end

  it "does not have multi_route match non-String named routes" do
    body('/foo/p').must_equal 'p'
    status('/foo/p/2').must_equal 404
  end

  it "has multi_route pick up routes newly added" do
    body('/foo/get/').must_equal 'get'
    status('/foo/delete').must_equal 404
    app.route('delete'){|r| r.on{'delete'}}
    body('/foo/delete').must_equal 'delete'
  end

  it "makes multi_route match longest route if multiple routes have the same prefix" do
    app.route("post/a"){|r| r.on{"pa2"}}
    app.route("get/a"){|r| r.on{"ga2"}}
    status('/foo').must_equal 404
    body('/foo/get/').must_equal 'get'
    body('/foo/get/a').must_equal 'ga2'
    body('/foo/post/').must_equal 'post'
    body('/foo/post/a').must_equal 'pa2'
    body('/foo/post/b').must_equal 'foo'
  end

  it "handles loading the plugin multiple times correctly" do
    app.plugin :multi_route
    body.must_equal 'get'
    body('REQUEST_METHOD'=>'POST').must_equal 'post'
    body('/a').must_equal 'geta'
    body('/a', 'REQUEST_METHOD'=>'POST').must_equal 'posta'
    body('/b').must_equal 'getb'
    body('/b', 'REQUEST_METHOD'=>'POST').must_equal 'postb'
    status('/c').must_equal 404
    status('/c', 'REQUEST_METHOD'=>'POST').must_equal 404
  end

  it "handles subclassing correctly" do
    @app = Class.new(@app)
    @app.route do |r|
      r.get do
        r.route("post")

        r.is "b" do
          "1b"
        end
      end
      r.post do
        r.route("get")

        r.is "b" do
          "2b"
        end
      end
    end

    body.must_equal 'post'
    body('REQUEST_METHOD'=>'POST').must_equal 'get'
    body('/a').must_equal 'posta'
    body('/a', 'REQUEST_METHOD'=>'POST').must_equal 'geta'
    body('/b').must_equal '1b'
    body('/b', 'REQUEST_METHOD'=>'POST').must_equal '2b'
    status('/c').must_equal 404
    status('/c', 'REQUEST_METHOD'=>'POST').must_equal 404
  end

  it "uses the named route return value in multi_route if no block is given" do
    app.route{|r| r.multi_route}
    body('/get').must_equal 'getd'
    body('/post').must_equal 'postd'
  end
end

describe "multi_route plugin" do
  it "r.multi_route works even without routes defined" do
    app(:multi_route) do |r|
      r.multi_route
      'a'
    end
    body.must_equal 'a'
  end
end

describe "multi_route plugin" do 
  before do
    app(:bare) do
      plugin :multi_route

      route("foo", "foo") do |r|
        "#{@p}ff"
      end

      route("bar", "foo") do |r|
        "#{@p}fb"
      end

      route("foo", "bar") do |r|
        "#{@p}bf"
      end

      route("bar", "bar") do |r|
        "#{@p}bb"
      end
    end
  end

  it "handles namespaces in r.route" do
    app.route("foo") do |r|
      @p = 'f'
      r.on("foo"){r.route("foo", "foo")}
      r.on("bar"){r.route("bar", "foo")}
      @p
    end

    app.route("bar") do |r|
      @p = 'b'
      r.on("foo"){r.route("foo", "bar")}
      r.on("bar"){r.route("bar", "bar")}
      @p
    end

    app.route do |r|
      r.on("foo"){r.route("foo")}
      r.on("bar"){r.route("bar")}
    end

    body('/foo').must_equal 'f'
    body('/foo/foo').must_equal 'fff'
    body('/foo/bar').must_equal 'ffb'
    body('/bar').must_equal 'b'
    body('/bar/foo').must_equal 'bbf'
    body('/bar/bar').must_equal 'bbb'
  end

  it "handles namespaces in r.multi_route" do
    app.route("foo") do |r|
      @p = 'f'
      r.multi_route("foo")
      @p
    end

    app.route("bar") do |r|
      @p = 'b'
      r.multi_route("bar")
      @p
    end

    app.route do |r|
      r.multi_route
    end

    body('/foo').must_equal 'f'
    body('/foo/foo').must_equal 'fff'
    body('/foo/bar').must_equal 'ffb'
    body('/bar').must_equal 'b'
    body('/bar/foo').must_equal 'bbf'
    body('/bar/bar').must_equal 'bbb'
  end
end
