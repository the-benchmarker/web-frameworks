require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

begin
  require 'erubis'
  require 'tilt'
  require 'tilt/erb'
  begin
    require 'tilt/erubis'
  rescue LoadError
    # Tilt 1 support
  end
rescue LoadError
  warn "tilt or erubis not installed, skipping _erubis_escaping plugin test"  
else
describe "_erubis_escaping plugin" do
  before do
    if defined?(Tilt::ErubisTemplate) && ::Tilt['erb'] != Tilt::ErubisTemplate
      # Set erubis as default erb template handler
      Tilt.register(Tilt::ErubisTemplate, 'erb')
    end
  end

  it "should escape inside <%= %> and not inside <%== %>, and handle postfix conditionals" do
    app(:bare) do
      plugin :render, :escape=>true

      route do |r|
        render(:inline=>'<%= "<>" %> <%== "<>" %><%= "<>" if false %>')
      end
    end

    body.must_equal '&lt;&gt; <>'
  end

  it "should consider classes in :escape_safe_classes as safe" do
    c = Class.new(String)
    c2 = Class.new(String)
    app(:bare) do
      plugin :render, :escape=>true, :escape_safe_classes=>c

      route do |r|
        @c, @c2 = c, c2
        render(:inline=>'<%= @c2.new("<>") %> <%= @c.new("<>") %>')
      end
    end

    body.must_equal '&lt;&gt; <>'
  end

  it "should covnert arguments to strings when escaping with safe classes" do
    app(:bare) do
      plugin :render, :escape=>true, :escape_safe_classes=>[]

      route do |r|
        render(:inline=>'<%= :"<>" %> <%== :"<>" %><%= :"<>" if false %>')
      end
    end

    body.must_equal '&lt;&gt; <>'
  end

  it "should allow use of custom :escaper" do
    escaper = Object.new
    def escaper.escape_xml(s)
      s.gsub("'", "''")
    end
    app(:bare) do
      plugin :render, :escape=>true, :escaper=>escaper

      route do |r|
        render(:inline=>'<%= "ab\'1" %> <%== "ab\'1" %>')
      end
    end

    body.must_equal "ab''1 ab'1"
  end

  it "should allow for per-branch escaping via set_view options" do
    app(:bare) do
      plugin :render, :escape=>true
      plugin :view_options

      route do |r|
        set_view_options :template_opts=>{:engine_class=>nil}
        r.is 'a' do
          set_view_options :template_opts=>{:engine_class=>render_opts[:template_opts][:engine_class]}
          render(:inline=>'<%= "<>" %>')
        end
        render(:inline=>'<%= "<>" %>')
      end
    end

    body('/a').must_equal '&lt;&gt;'
    body.must_equal '<>'
  end
end
end
