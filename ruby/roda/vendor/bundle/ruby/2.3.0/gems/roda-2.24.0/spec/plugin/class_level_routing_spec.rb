require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "class_level_routing plugin" do 
  before do
    app(:bare) do 
      plugin :class_level_routing
      plugin :all_verbs

      root do
        'root'
      end

      on "foo" do
        request.get "bar" do
          "foobar"
        end

        "foo"
      end

      is "d", :d do |x|
        request.get do
          "bazget#{x}"
        end

        request.post do
          "bazpost#{x}"
        end
      end

      meths = %w'get post delete head options patch put trace'
      meths.concat(%w'link unlink') if ::Rack::Request.method_defined?("link?")
      meths.each do |meth|
        send(meth, :d) do |m|
          "x-#{meth}-#{m}"
        end
      end
    end
  end

  it "adds class methods for setting up routes" do
    body.must_equal 'root'
    body('/foo').must_equal 'foo'
    body('/foo/bar').must_equal 'foobar'
    body('/d/go').must_equal 'bazgetgo'
    body('/d/go', 'REQUEST_METHOD'=>'POST').must_equal 'bazpostgo'
    body('/bar').must_equal "x-get-bar"
    body('/bar', 'REQUEST_METHOD'=>'POST').must_equal "x-post-bar"
    body('/bar', 'REQUEST_METHOD'=>'DELETE').must_equal "x-delete-bar"
    body('/bar', 'REQUEST_METHOD'=>'HEAD').must_equal "x-head-bar"
    body('/bar', 'REQUEST_METHOD'=>'OPTIONS').must_equal "x-options-bar"
    body('/bar', 'REQUEST_METHOD'=>'PATCH').must_equal "x-patch-bar"
    body('/bar', 'REQUEST_METHOD'=>'PUT').must_equal "x-put-bar"
    body('/bar', 'REQUEST_METHOD'=>'TRACE').must_equal "x-trace-bar"
    if ::Rack::Request.method_defined?("link?")
      body('/bar', 'REQUEST_METHOD'=>'LINK').must_equal "x-link-bar"
      body('/bar', 'REQUEST_METHOD'=>'UNLINK').must_equal "x-unlink-bar"
    end

    status.must_equal 200
    status("/asdfa/asdf").must_equal 404

    @app = Class.new(app)
    body.must_equal 'root'
    body('/foo').must_equal 'foo'
    body('/foo/bar').must_equal 'foobar'
    body('/d/go').must_equal 'bazgetgo'
    body('/d/go', 'REQUEST_METHOD'=>'POST').must_equal 'bazpostgo'
    body('/bar').must_equal "x-get-bar"
    body('/bar', 'REQUEST_METHOD'=>'POST').must_equal "x-post-bar"
    body('/bar', 'REQUEST_METHOD'=>'DELETE').must_equal "x-delete-bar"
    body('/bar', 'REQUEST_METHOD'=>'HEAD').must_equal "x-head-bar"
    body('/bar', 'REQUEST_METHOD'=>'OPTIONS').must_equal "x-options-bar"
    body('/bar', 'REQUEST_METHOD'=>'PATCH').must_equal "x-patch-bar"
    body('/bar', 'REQUEST_METHOD'=>'PUT').must_equal "x-put-bar"
    body('/bar', 'REQUEST_METHOD'=>'TRACE').must_equal "x-trace-bar"
  end

  it "only calls class level routes if routing tree doesn't handle request" do
    app.route do |r|
      r.root do
        'iroot'
      end

      r.get 'foo' do
        'ifoo'
      end

      r.on 'bar' do
        r.get true do
          response.status = 404
          ''
        end
        r.post true do
          'ibar'
        end
      end
    end

    body.must_equal 'iroot'
    body('/foo').must_equal 'ifoo'
    body('/foo/bar').must_equal 'foobar'
    body('/d/go').must_equal 'bazgetgo'
    body('/d/go', 'REQUEST_METHOD'=>'POST').must_equal 'bazpostgo'
    body('/bar').must_equal ""
    body('/bar', 'REQUEST_METHOD'=>'POST').must_equal "ibar"
    body('/bar', 'REQUEST_METHOD'=>'DELETE').must_equal "x-delete-bar"
    body('/bar', 'REQUEST_METHOD'=>'HEAD').must_equal "x-head-bar"
    body('/bar', 'REQUEST_METHOD'=>'OPTIONS').must_equal "x-options-bar"
    body('/bar', 'REQUEST_METHOD'=>'PATCH').must_equal "x-patch-bar"
    body('/bar', 'REQUEST_METHOD'=>'PUT').must_equal "x-put-bar"
    body('/bar', 'REQUEST_METHOD'=>'TRACE').must_equal "x-trace-bar"
  end

  it "works with the not_found plugin if loaded before" do
    app.plugin :not_found do
      "nf"
    end

    body.must_equal 'root'
    body('/foo').must_equal 'foo'
    body('/foo/bar').must_equal 'foobar'
    body('/d/go').must_equal 'bazgetgo'
    body('/d/go', 'REQUEST_METHOD'=>'POST').must_equal 'bazpostgo'
    body('/bar').must_equal "x-get-bar"
    body('/bar', 'REQUEST_METHOD'=>'POST').must_equal "x-post-bar"
    body('/bar', 'REQUEST_METHOD'=>'DELETE').must_equal "x-delete-bar"
    body('/bar', 'REQUEST_METHOD'=>'HEAD').must_equal "x-head-bar"
    body('/bar', 'REQUEST_METHOD'=>'OPTIONS').must_equal "x-options-bar"
    body('/bar', 'REQUEST_METHOD'=>'PATCH').must_equal "x-patch-bar"
    body('/bar', 'REQUEST_METHOD'=>'PUT').must_equal "x-put-bar"
    body('/bar', 'REQUEST_METHOD'=>'TRACE').must_equal "x-trace-bar"

    status.must_equal 200
    status("/asdfa/asdf").must_equal 404
    body("/asdfa/asdf").must_equal "nf"
  end

  it "works when freezing the app" do
    app.freeze
    body.must_equal 'root'
    body('/foo').must_equal 'foo'
    body('/foo/bar').must_equal 'foobar'
    body('/d/go').must_equal 'bazgetgo'
    body('/d/go', 'REQUEST_METHOD'=>'POST').must_equal 'bazpostgo'
    body('/bar').must_equal "x-get-bar"
    body('/bar', 'REQUEST_METHOD'=>'POST').must_equal "x-post-bar"
    body('/bar', 'REQUEST_METHOD'=>'DELETE').must_equal "x-delete-bar"
    body('/bar', 'REQUEST_METHOD'=>'HEAD').must_equal "x-head-bar"
    body('/bar', 'REQUEST_METHOD'=>'OPTIONS').must_equal "x-options-bar"
    body('/bar', 'REQUEST_METHOD'=>'PATCH').must_equal "x-patch-bar"
    body('/bar', 'REQUEST_METHOD'=>'PUT').must_equal "x-put-bar"
    body('/bar', 'REQUEST_METHOD'=>'TRACE').must_equal "x-trace-bar"
    if ::Rack::Request.method_defined?("link?")
      body('/bar', 'REQUEST_METHOD'=>'LINK').must_equal "x-link-bar"
      body('/bar', 'REQUEST_METHOD'=>'UNLINK').must_equal "x-unlink-bar"
    end

    status.must_equal 200
    status("/asdfa/asdf").must_equal 404

    proc{app.on{}}.must_raise
  end
end
