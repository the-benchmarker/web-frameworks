require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "path plugin" do 
  def path_app(*args, &block)
    app(:bare) do
      plugin :path
      path(*args, &block)
      route{|r| send(r.path_info)}
    end
  end

  def path_script_name_app(*args, &block)
    app(:bare) do
      opts[:add_script_name] = true
      plugin :path
      path(*args, &block)
      route{|r| send(r.path_info)}
    end
  end

  def path_block_app(b, *args, &block)
    path_app(*args, &block)
    app.route{|r| send(r.path_info, &b)}
  end

  it "adds path method for defining named paths" do
    app(:bare) do
      plugin :path
      path :foo, "/foo"
      path :bar do |o|
        "/bar/#{o}"
      end
      path :baz do |&block|
        "/baz/#{block.call}"
      end

      route do |r|
        "#{foo_path}#{bar_path('a')}#{baz_path{'b'}}"
      end
    end

    body.must_equal '/foo/bar/a/baz/b'
  end

  it "raises if both path and block are given" do
    app.plugin :path
    proc{app.path(:foo, '/foo'){}}.must_raise(Roda::RodaError)
  end

  it "raises if neither path nor block are given" do
    app.plugin :path
    proc{app.path(:foo)}.must_raise(Roda::RodaError)
  end

  it "raises if two options hashes are given" do
    app.plugin :path
    proc{app.path(:foo, {:name=>'a'}, :add_script_name=>true)}.must_raise(Roda::RodaError)
  end

  it "supports :name option for naming the method" do
    path_app(:foo, :name=>'foobar_route'){"/bar/foo"}
    body("foobar_route").must_equal "/bar/foo"
  end

  it "supports :add_script_name option for automatically adding the script name" do
    path_app(:foo, :add_script_name=>true){"/bar/foo"}
    body("foo_path", 'SCRIPT_NAME'=>'/baz').must_equal "/baz/bar/foo"
  end

  it "respects :add_script_name app option for automatically adding the script name" do
    path_script_name_app(:foo){"/bar/foo"}
    body("foo_path", 'SCRIPT_NAME'=>'/baz').must_equal "/baz/bar/foo"
  end

  it "supports :add_script_name=>false option for not automatically adding the script name" do
    path_script_name_app(:foo, :add_script_name=>false){"/bar/foo"}
    body("foo_path", 'SCRIPT_NAME'=>'/baz').must_equal "/bar/foo"
  end

  it "supports path method accepting a block when using :add_script_name" do
    path_block_app(lambda{"c"}, :foo, :add_script_name=>true){|&block| "/bar/foo/#{block.call}"}
    body("foo_path", 'SCRIPT_NAME'=>'/baz').must_equal "/baz/bar/foo/c"
  end

  it "supports :url option for also creating a *_url method" do
    path_app(:foo, :url=>true){"/bar/foo"}
    body("foo_path", 'HTTP_HOST'=>'example.org', "rack.url_scheme"=>'http', 'SERVER_PORT'=>80).must_equal "/bar/foo"
    body("foo_url", 'HTTP_HOST'=>'example.org', "rack.url_scheme"=>'http', 'SERVER_PORT'=>80).must_equal "http://example.org/bar/foo"
  end

  it "supports url method accepting a block when using :url" do
    path_block_app(lambda{"c"}, :foo, :url=>true){|&block| "/bar/foo/#{block.call}"}
    body("foo_url", 'HTTP_HOST'=>'example.org', "rack.url_scheme"=>'http', 'SERVER_PORT'=>80).must_equal "http://example.org/bar/foo/c"
  end

  it "supports url method name specified in :url option" do
    path_app(:foo, :url=>:foobar_uri){"/bar/foo"}
    body("foo_path", 'HTTP_HOST'=>'example.org', "rack.url_scheme"=>'http', 'SERVER_PORT'=>80).must_equal "/bar/foo"
    body("foobar_uri", 'HTTP_HOST'=>'example.org', "rack.url_scheme"=>'http', 'SERVER_PORT'=>80).must_equal "http://example.org/bar/foo"
  end

  it "supports :url_only option for not creating a path method" do
    path_app(:foo, :url_only=>true){"/bar/foo"}
    proc{body("foo_path")}.must_raise(NoMethodError)
    body("foo_url", 'HTTP_HOST'=>'example.org', "rack.url_scheme"=>'http', 'SERVER_PORT'=>80).must_equal "http://example.org/bar/foo"
  end

  it "handles non-default ports in url methods" do
    path_app(:foo, :url=>true){"/bar/foo"}
    body("foo_url", 'HTTP_HOST'=>'example.org', "rack.url_scheme"=>'http', 'SERVER_PORT'=>81).must_equal "http://example.org:81/bar/foo"
  end
end

describe "path plugin" do 
  before do
    app(:bare) do
      plugin :path
      route{|r| path(*env['path'])}
    end


    c = Class.new{attr_accessor :a}
    app.path(c){|obj, *args| "/d/#{obj.a}/#{File.join(*args)}"}
    @obj = c.new
    @obj.a = 1
  end

  it "Roda#path respects classes and symbols registered via Roda.path" do
    # Strings
    body('path'=>'/foo/bar').must_equal '/foo/bar'

    # Classes
    body('path'=>@obj).must_equal '/d/1/'
    body('path'=>[@obj, 'foo']).must_equal '/d/1/foo'
    body('path'=>[@obj, 'foo', 'bar']).must_equal '/d/1/foo/bar'
  end

  it "Roda#path raises an error for an unrecognized class" do
    # Strings
    proc{body('path'=>:foo)}.must_raise(Roda::RodaError)
  end

  it "Roda#path respects :add_script_name app option" do
    app.opts[:add_script_name] = true

    # Strings
    body('path'=>'/foo/bar', 'SCRIPT_NAME'=>'/baz').must_equal '/baz/foo/bar'

    # Classes
    body('path'=>@obj, 'SCRIPT_NAME'=>'/baz').must_equal '/baz/d/1/'
    body('path'=>[@obj, 'foo'], 'SCRIPT_NAME'=>'/baz').must_equal '/baz/d/1/foo'
    body('path'=>[@obj, 'foo', 'bar'], 'SCRIPT_NAME'=>'/baz').must_equal '/baz/d/1/foo/bar'
  end

  it "Roda#path works in subclasses" do
    old_app = @app
    @app = Class.new(@app)
    @app.route{|r| path('/a')}
    body.must_equal '/a'

    @app.path(String){|b| "/foo#{b}"}
    body.must_equal '/foo/a'

    @app = old_app
    body('path'=>'/a').must_equal '/a'
  end

  it "registers classes by reference by default" do
    c1 = Class.new
    def c1.name; 'C'; end
    c2 = Class.new
    def c2.name; 'C'; end
    @app.path(c1){'/c'}
    @app.route{|r| path(r.env['c'])}
    body('c'=>c1.new).must_equal '/c'
    proc{body('c'=>c2.new)}.must_raise(Roda::RodaError)
  end

  it ":by_name => option registers classes by name" do
    c1 = Class.new
    def c1.name; 'C'; end
    c2 = Class.new
    def c2.name; 'C'; end
    @app.plugin :path, :by_name=>true
    @app.path(c1){'/c'}
    @app.route{|r| path(r.env['c'])}
    body('c'=>c1.new).must_equal '/c'
    body('c'=>c2.new).must_equal '/c'
  end

  it ":by_name defaults to true in development" do
    with_rack_env('development') do
      app(:path){}
    end
    app.opts[:path_class_by_name].must_equal true
    app(:path){}
    app.opts[:path_class_by_name].must_equal false
  end

  it "Roda.path_block returns the block used" do
    c = Class.new
    b = proc{|x| x.to_s}
    @app.path(c, &b)
    # Work around minitest bug
    app.path_block(c).must_equal b
  end

  it "Roda.path doesn't work with classes without blocks" do
    proc{app.path(Class.new)}.must_raise(Roda::RodaError)
  end

  it "Roda.path doesn't work with classes with paths or options" do
    proc{app.path(Class.new, '/a'){}}.must_raise(Roda::RodaError)
    proc{app.path(Class.new, nil, :a=>1){}}.must_raise(Roda::RodaError)
  end

  it "Roda.path doesn't work after freezing the app" do
    app.freeze
    proc{app.path(Class.new){|obj| ''}}.must_raise
  end
end

