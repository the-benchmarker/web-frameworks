require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "strip_path_prefix plugin" do 
  it "strips path prefix when expanding paths" do
    app(:bare){}
    abs_dir = app.expand_path('spec')

    app.plugin :strip_path_prefix
    app.expand_path('spec').must_equal 'spec'
    File.expand_path(app.expand_path('spec'), Dir.pwd).must_equal abs_dir

    app.expand_path('/foo').must_equal '/foo'
    app.expand_path('bar', '/foo').must_equal '/foo/bar'

    app.opts[:root] = '/foo'
    app.expand_path('bar').must_equal '/foo/bar'
    app.plugin :strip_path_prefix, '/foo'
    app.expand_path('bar').must_equal 'bar'

    app.opts[:root] = '/foo/bar'
    app.expand_path('baz').must_equal 'bar/baz'
  end
end

