require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

begin
  require 'tilt/erb'
rescue LoadError
  warn "tilt not installed, skipping view_options plugin test"  
else
describe "view_options plugin view subdirs" do
  before do
    app(:bare) do
      plugin :render, :views=>"."
      plugin :view_options

      route do |r|
        append_view_subdir 'spec' 

        r.on "home" do
          set_view_subdir 'spec/views'
          view("home", :locals=>{:name => "Agent Smith", :title => "Home"}, :layout_opts=>{:locals=>{:title=>"Home"}})
        end

        r.on "about" do
          append_view_subdir 'views'
          render("about", :locals=>{:title => "About Roda"})
        end

        r.on "path" do
          render('spec/views/about', :locals=>{:title => "Path"}, :layout_opts=>{:locals=>{:title=>"Home"}})
        end
      end
    end
  end

  it "should use set subdir if template name does not contain a slash" do
    body("/home").strip.must_equal "<title>Roda: Home</title>\n<h1>Home</h1>\n<p>Hello Agent Smith</p>"
  end

  it "should not use set subdir if template name contains a slash" do
    body("/about").strip.must_equal "<h1>About Roda</h1>"
  end

  it "should not change behavior when subdir is not set" do
    body("/path").strip.must_equal "<h1>Path</h1>"
  end
end

describe "view_options plugin" do
  it "should set view and layout options and locals to use" do
    app(:view_options) do
      set_view_options :views=>'spec/views'
      set_view_locals :title=>'About Roda'
      set_layout_options :views=>'spec/views', :template=>'layout-alternative'
      set_layout_locals :title=>'Home'
      view('about')
    end

    body.strip.must_equal "<title>Alternative Layout: Home</title>\n<h1>About Roda</h1>"
  end

  it "should merge multiple calls to set view and layout options and locals" do
    app(:view_options) do
      set_layout_options :views=>'spec/views', :template=>'multiple-layout', :ext=>'str'
      set_view_options :views=>'spec/views', :ext=>'str'
      set_layout_locals :title=>'About Roda'
      set_view_locals :title=>'Home'

      set_layout_options :ext=>'erb'
      set_view_options :ext=>'erb'
      set_layout_locals :a=>'A'
      set_view_locals :b=>'B'

      view('multiple')
    end

    body.strip.must_equal "About Roda:A::Home:B"
  end

  it "should have set_view_locals have more precedence than plugin options, but less than view/render method options" do
    app(:bare) do 
      plugin :render, :views=>"./spec/views", :locals=>{:title=>'Home', :b=>'B'}, :layout_opts=>{:template=>'multiple-layout', :locals=>{:title=>'About Roda', :a=>'A'}}
      plugin :view_options

      route do |r|
        r.is 'c' do
          view(:multiple)
        end

        set_view_locals :b=>'BB'
        set_layout_locals :a=>'AA'

        r.on 'b' do
          set_view_locals :title=>'About'
          set_layout_locals :title=>'Roda'

          r.is 'a' do
            view(:multiple)
          end

          view("multiple", :locals=>{:b => "BBB"}, :layout_opts=>{:locals=>{:a=>'AAA'}})
        end

        r.is 'a' do
          view(:multiple)
        end

        view("multiple", :locals=>{:b => "BBB"}, :layout_opts=>{:locals=>{:a=>'AAA'}})
      end
    end

    body('/c').strip.must_equal "About Roda:A::Home:B"
    body('/b/a').strip.must_equal "Roda:AA::About:BB"
    body('/b').strip.must_equal "Roda:AAA::About:BBB"
    body('/a').strip.must_equal "About Roda:AA::Home:BB"
    body.strip.must_equal "About Roda:AAA::Home:BBB"
  end
end
end
