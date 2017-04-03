require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

begin
  require 'tilt/erb'
rescue LoadError
  warn "tilt not installed, skipping named_templates plugin test"  
else
describe "named_templates plugin" do 
  it "adds template method method for naming templates, and have render recognize it" do
    app(:bare) do
      plugin :named_templates

      template :foo do
        @b = 2
        "foo<%= @a %><%= @b %>"
      end
      template :layout, :engine=>:str do
        @c = 3
        'bar#{@a}#{@c}-#{yield}-baz'
      end

      route do |r|
        @a = 1
        view(:foo)
      end
    end

    body.must_equal 'bar13-foo12-baz'
    @app = Class.new(@app)
    body.must_equal 'bar13-foo12-baz'
  end

  it "works when freezing the app" do
    app(:bare) do
      plugin :named_templates

      template :foo do
        @b = 2
        "foo<%= @a %><%= @b %>"
      end
      template :layout, :engine=>:str do
        @c = 3
        'bar#{@a}#{@c}-#{yield}-baz'
      end

      route do |r|
        @a = 1
        view(:foo)
      end
    end

    app.freeze
    body.must_equal 'bar13-foo12-baz'

    proc{app.template(:b){"a"}}.must_raise
  end

  it "works with the view_subdirs plugin" do
    app(:bare) do
      plugin :render
      plugin :view_subdirs
      plugin :named_templates

      template "foo/bar" do
        @b = 2
        "foobar<%= @a %><%= @b %>"
      end
      template "foo/layout", :engine=>:str do
        @c = 3
        'foo#{@a}#{@c}-#{yield}-baz'
      end
      template "bar/layout", :engine=>:str do
        @c = 3
        'bar#{@a}#{@c}-#{yield}-baz'
      end

      route do |r|
        r.is 'foo' do
          set_view_subdir :foo
          @a = 1
          view(:bar)
        end
        r.is 'bar' do
          set_view_subdir :bar
          @a = 4
          @b = 2
          view(:inline=>"barfoo<%= @a %><%= @b %>")
        end
      end
    end

    body('/foo').must_equal 'foo13-foobar12-baz'
    body('/bar').must_equal 'bar43-barfoo42-baz'
  end
end
end
