require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

begin
  require 'tilt/erb'
  require 'tilt/string'
rescue LoadError
  warn "tilt not installed, skipping render plugin test"  
else
describe "render plugin" do
  before do
    app(:bare) do
      plugin :render, :views=>"./spec/views", :check_paths=>true

      route do |r|
        r.on "home" do
          view("home", :locals=>{:name => "Agent Smith", :title => "Home"}, :layout_opts=>{:locals=>{:title=>"Home"}})
        end

        r.on "about" do
          render("about", :locals=>{:title => "About Roda"})
        end

        r.on "inline" do
          view(:inline=>"Hello <%= name %>", :locals=>{:name => "Agent Smith"}, :layout=>nil)
        end

        r.on "path" do
          render(:path=>"./spec/views/about.erb", :locals=>{:title => "Path"}, :layout_opts=>{:locals=>{:title=>"Home"}})
        end

        r.on "content" do
          view(:content=>'bar', :layout_opts=>{:locals=>{:title=>"Home"}})
        end
      end
    end
  end

  it "default actions" do
    body("/about").strip.must_equal "<h1>About Roda</h1>"
    body("/home").strip.must_equal "<title>Roda: Home</title>\n<h1>Home</h1>\n<p>Hello Agent Smith</p>"
    body("/inline").strip.must_equal "Hello Agent Smith"
    body("/path").strip.must_equal "<h1>Path</h1>"
    body("/content").strip.must_equal "<title>Roda: Home</title>\nbar"
  end

  it "with str as engine" do
    app.plugin :render, :engine => "str"
    body("/about").strip.must_equal "<h1>About Roda</h1>"
    body("/home").strip.must_equal "<title>Roda: Home</title>\n<h1>Home</h1>\n<p>Hello Agent Smith</p>"
    body("/inline").strip.must_equal "Hello <%= name %>"
  end

  it "with str as ext" do
    app.plugin :render, :ext => "str"
    body("/about").strip.must_equal "<h1>About Roda</h1>"
    body("/home").strip.must_equal "<title>Roda: Home</title>\n<h1>Home</h1>\n<p>Hello Agent Smith</p>"
    body("/inline").strip.must_equal "Hello <%= name %>" 
  end

  it "custom default layout support" do
    app.plugin :render, :layout => "layout-alternative"
    body("/home").strip.must_equal "<title>Alternative Layout: Home</title>\n<h1>Home</h1>\n<p>Hello Agent Smith</p>"
  end

  it "using hash for :layout" do
    app.plugin :render, :layout => {:inline=> 'a<%= yield %>b'}
    body("/home").strip.must_equal "a<h1>Home</h1>\n<p>Hello Agent Smith</p>\nb"
  end
end

describe "render plugin with :layout_opts=>{:merge_locals=>true}" do
  before do
    app(:bare) do
      plugin :render, :views=>"./spec/views", :check_paths=>true, :locals=>{:a=>1, :b=>2, :c=>3, :d=>4, :e=>5}, :layout_opts=>{:inline=>'<%= a %>|<%= b %>|<%= c %>|<%= d %>|<%= e %>|<%= f %>|<%= yield %>', :merge_locals=>true, :locals=>{:a=>-1, :f=>6}}

      route do |r|
        r.on "base" do
          view(:inline=>'(<%= a %>|<%= b %>|<%= c %>|<%= d %>|<%= e %>)')
        end
        r.on "override" do
          view(:inline=>'(<%= a %>|<%= b %>|<%= c %>|<%= d %>|<%= e %>)', :locals=>{:b=>-2, :d=>-4, :f=>-6}, :layout_opts=>{:locals=>{:d=>0, :c=>-3, :e=>-5}})
        end
        r.on "no_merge" do
          view(:inline=>'(<%= a %>|<%= b %>|<%= c %>|<%= d %>|<%= e %>)', :locals=>{:b=>-2, :d=>-4, :f=>-6}, :layout_opts=>{:merge_locals=>false, :locals=>{:d=>0, :c=>-3, :e=>-5}})
        end
      end
    end
  end

  it "should choose method opts before plugin opts, and layout specific before locals" do
    body("/base").must_equal '-1|2|3|4|5|6|(1|2|3|4|5)'
    body("/override").must_equal '-1|-2|-3|0|-5|-6|(1|-2|3|-4|5)'
    body("/no_merge").must_equal '-1|2|-3|0|-5|6|(1|-2|3|-4|5)'
  end
end

describe "render plugin" do
  it "simple layout support" do
    app(:bare) do
      plugin :render
      
      route do |r|
        render(:path=>"spec/views/layout-yield.erb") do
          render(:path=>"spec/views/content-yield.erb")
        end
      end
    end

    body.gsub(/\n+/, "\n").must_equal "Header\nThis is the actual content.\nFooter\n"
  end

  it "views without default layouts" do
    app(:bare) do
      plugin :render, :views=>"./spec/views", :layout=>false
      
      route do |r|
        view("home", :locals=>{:name=>"Agent Smith", :title=>"Home"})
      end
    end

    body.strip.must_equal "<h1>Home</h1>\n<p>Hello Agent Smith</p>"
  end

  it "layout overrides" do
    app(:bare) do
      plugin :render, :views=>"./spec/views"
      
      route do |r|
        view("home", :locals=>{:name=>"Agent Smith", :title=>"Home" }, :layout=>"layout-alternative", :layout_opts=>{:locals=>{:title=>"Home"}})
      end
    end

    body.strip.must_equal "<title>Alternative Layout: Home</title>\n<h1>Home</h1>\n<p>Hello Agent Smith</p>"
  end

  it "locals overrides" do
    app(:bare) do
      plugin :render, :views=>"./spec/views", :locals=>{:title=>'Home', :b=>'B'}, :layout_opts=>{:template=>'multiple-layout', :locals=>{:title=>'Roda', :a=>'A'}}
      
      route do |r|
        view("multiple", :locals=>{:b=>"BB"}, :layout_opts=>{:locals=>{:a=>'AA'}})
      end
    end

    body.strip.must_equal "Roda:AA::Home:BB"
  end

  it ":layout=>true/false/string/hash/not-present respects plugin layout switch and template" do
    app(:bare) do
      plugin :render, :views=>"./spec/views", :layout_opts=>{:template=>'layout-yield', :locals=>{:title=>'a'}}
      
      route do |r|
        opts = {:content=>'bar'}
        opts[:layout] = true if r.path == '/'
        opts[:layout] = false if r.path == '/f'
        opts[:layout] = 'layout' if r.path == '/s'
        opts[:layout] = {:template=>'layout'} if r.path == '/h'
        view(opts)
      end
    end

    body.gsub("\n", '').must_equal "HeaderbarFooter"
    body('/a').gsub("\n", '').must_equal "HeaderbarFooter"
    body('/f').gsub("\n", '').must_equal "bar"
    body('/s').gsub("\n", '').must_equal "<title>Roda: a</title>bar"
    body('/h').gsub("\n", '').must_equal "<title>Roda: a</title>bar"

    app.plugin :render
    body.gsub("\n", '').must_equal "HeaderbarFooter"
    body('/a').gsub("\n", '').must_equal "HeaderbarFooter"
    body('/f').gsub("\n", '').must_equal "bar"
    body('/s').gsub("\n", '').must_equal "<title>Roda: a</title>bar"
    body('/h').gsub("\n", '').must_equal "<title>Roda: a</title>bar"

    app.plugin :render, :layout=>true
    body.gsub("\n", '').must_equal "HeaderbarFooter"
    body('/a').gsub("\n", '').must_equal "HeaderbarFooter"
    body('/f').gsub("\n", '').must_equal "bar"
    body('/s').gsub("\n", '').must_equal "<title>Roda: a</title>bar"
    body('/h').gsub("\n", '').must_equal "<title>Roda: a</title>bar"

    app.plugin :render, :layout=>'layout-alternative'
    body.gsub("\n", '').must_equal "<title>Alternative Layout: a</title>bar"
    body('/a').gsub("\n", '').must_equal "<title>Alternative Layout: a</title>bar"
    body('/f').gsub("\n", '').must_equal "bar"
    body('/s').gsub("\n", '').must_equal "<title>Roda: a</title>bar"
    body('/h').gsub("\n", '').must_equal "<title>Roda: a</title>bar"

    app.plugin :render, :layout=>nil
    body.gsub("\n", '').must_equal "HeaderbarFooter"
    body('/a').gsub("\n", '').must_equal "bar"
    body('/f').gsub("\n", '').must_equal "bar"
    body('/s').gsub("\n", '').must_equal "<title>Roda: a</title>bar"
    body('/h').gsub("\n", '').must_equal "<title>Roda: a</title>bar"

    app.plugin :render, :layout=>false
    body.gsub("\n", '').must_equal "HeaderbarFooter"
    body('/a').gsub("\n", '').must_equal "bar"
    body('/f').gsub("\n", '').must_equal "bar"
    body('/s').gsub("\n", '').must_equal "<title>Roda: a</title>bar"
    body('/h').gsub("\n", '').must_equal "<title>Roda: a</title>bar"

    app.plugin :render, :layout_opts=>{:template=>'layout-alternative', :locals=>{:title=>'a'}}
    body.gsub("\n", '').must_equal "<title>Alternative Layout: a</title>bar"
    body('/a').gsub("\n", '').must_equal "bar"
    body('/f').gsub("\n", '').must_equal "bar"
    body('/s').gsub("\n", '').must_equal "<title>Roda: a</title>bar"
    body('/h').gsub("\n", '').must_equal "<title>Roda: a</title>bar"
  end

  it "app :root option affects :views default" do
    app
    app.plugin :render
    app.render_opts[:views].must_equal File.join(Dir.pwd, 'views')

    app.opts[:root] = '/foo'
    app.plugin :render
    # Work around for Windows
    app.render_opts[:views].sub(/\A\w:/, '').must_equal '/foo/views'

    app.opts[:root] = '/foo/bar'
    app.plugin :render
    app.render_opts[:views].sub(/\A\w:/, '').must_equal '/foo/bar/views'

    app.opts[:root] = nil
    app.plugin :render
    app.render_opts[:views].must_equal File.join(Dir.pwd, 'views')
    app.plugin :render, :views=>'bar'
    app.render_opts[:views].must_equal File.join(Dir.pwd, 'bar')
  end

  it "inline layouts and inline views" do
    app(:render) do
      view({:inline=>'bar'}, :layout=>{:inline=>'Foo: <%= yield %>'})
    end

    body.strip.must_equal "Foo: bar"
  end

  it "inline renders with opts" do
    app(:render) do
      render({:inline=>'<%= bar %>'}, {:engine=>'str'})
    end

    body.strip.must_equal '<%= bar %>'
  end

  it "template renders with :template opts" do
    app(:bare) do
      plugin :render, :views => "./spec/views"
      route do
        render(:template=>"about", :locals=>{:title => "About Roda"})
      end
    end
    body.strip.must_equal "<h1>About Roda</h1>"
  end

  it "template renders with :template_class opts" do
    app(:render) do
      @a = 1
      render(:inline=>'i#{@a}', :template_class=>::Tilt[:str])
    end
    body.must_equal "i1"
  end

  it "can specify engine-specific options via :engine_opts" do
    app(:bare) do
      plugin :render, :engine_opts=>{'a.erb'=>{:outvar=>'@a'}}
      route do |r|
        @a = nil
        r.is('a') do
          render(:inline=>'<%= @a.class.name %>', :engine=>'a.erb')
        end
        render(:inline=>'<%= @a.class.name %>')
      end
    end

    body('/a').must_equal "String"
    body.must_equal "NilClass"
  end

  it "template cache respects :template_opts" do
    c = Class.new do 
      def initialize(path, _, opts)
        @path = path
        @opts = opts
      end
      def render(*)
        "#{@path}-#{@opts[:foo]}"
      end
    end

    app(:render) do |r|
      r.is "a" do
        render(:inline=>"i", :template_class=>c, :template_opts=>{:foo=>'a'})
      end
      r.is "b" do
        render(:inline=>"i", :template_class=>c, :template_opts=>{:foo=>'b'})
      end
    end

    body('/a').must_equal "i-a"
    body('/b').must_equal "i-b"
  end

  it "template cache respects :template_block" do
    c = Class.new do 
      def initialize(path, *, &block)
        @path = path
        @block = block
      end
      def render(*)
        "#{@path}-#{@block.call}"
      end
    end 

    proca = proc{'a'}
    procb = proc{'b'}

    app(:render) do |r|
      r.is "a" do
        render(:path=>"i", :template_class=>c, :template_block=>proca)
      end
      r.is "b" do
        render(:path=>"i", :template_class=>c, :template_block=>procb)
      end
    end

    body('/a').must_equal "i-a"
    body('/b').must_equal "i-b"
  end

  it "template cache respects :locals" do
    template = '<%= @a ? b : c %>'

    app(:render) do |r|
      r.is "a" do
        @a = true
        render(:inline=>template.dup, :locals=>{:b=>1})
      end
      r.is "b" do
        @a = true
        render(:inline=>template.dup, :locals=>{:b=>2, :c=>4})
      end
      r.is "c" do
        @a = nil
        render(:inline=>template.dup, :locals=>{:c=>3})
      end
    end

    body('/a').must_equal "1"
    body('/b').must_equal "2"
    body('/c').must_equal "3"
  end

  it "Default to :explicit_cache=>true in development mode" do
    with_rack_env('development') do
      app(:render){}
    end
    app.render_opts[:explicit_cache].must_equal true
    app(:render){}
    app.render_opts[:explicit_cache].must_equal false
  end

  it "Support :cache=>false option to disable template caching" do
    app(:bare) do
      plugin :render, :views=>"./spec/views"

      route do |r|
        @a = 'a'
        r.is('a'){render('iv', :cache=>false)}
        render('iv')
      end
    end

    body('/a').strip.must_equal "a"
    app.render_opts[:cache][File.expand_path('spec/views/iv.erb')].must_be_nil
    body('/b').strip.must_equal "a"
    app.render_opts[:cache][File.expand_path('spec/views/iv.erb')].wont_equal nil
  end

  it "Support :cache=>false option to disable template caching even when :cache_key is given" do
    app(:bare) do
      plugin :render, :views=>"./spec/views"

      route do |r|
        @a = 'a'
        r.is('a'){render('iv', :cache=>false, :cache_key=>:foo)}
        render('iv', :cache_key=>:foo)
      end
    end

    body('/a').strip.must_equal "a"
    app.render_opts[:cache][:foo].must_be_nil
    body('/b').strip.must_equal "a"
    app.render_opts[:cache][:foo].wont_equal nil
  end

  it "Support :explicit_cache option to disable caching by default, but still allow caching on a per-call basis" do
    app(:bare) do
      plugin :render, :views=>"./spec/views", :explicit_cache=>true

      route do |r|
        @a = 'a'
        r.is('a'){render('iv')}
        render('iv', :cache=>true)
      end
    end

    body('/a').strip.must_equal "a"
    app.render_opts[:cache][File.expand_path('spec/views/iv.erb')].must_be_nil
    body('/b').strip.must_equal "a"
    app.render_opts[:cache][File.expand_path('spec/views/iv.erb')].wont_equal nil
  end

  it "Support :explicit_cache plugin option with :cache_key render option" do
    app(:bare) do
      plugin :render, :views=>"./spec/views", :explicit_cache=>true

      route do |r|
        @a = 'a'
        r.is('a'){render('iv', :cache_key=>:foo)}
        render('iv', :cache=>true, :cache_key=>:foo)
      end
    end

    body('/a').strip.must_equal "a"
    app.render_opts[:cache][:foo].must_be_nil
    body('/b').strip.must_equal "a"
    app.render_opts[:cache][:foo].wont_equal nil
  end

  it "Support :cache=>true option to enable template caching when :template_block is used" do
    c = Class.new do 
      def initialize(path, *, &block)
        @path = path
        @block = block
      end
      def render(*)
        "#{@path}-#{@block.call}"
      end
    end 

    proca = proc{'a'}

    app(:bare) do
      plugin :render, :views=>"./spec/views"

      route do |r|
        @a = 'a'
        r.is('a'){render(:path=>'iv', :template_class=>c, :template_block=>proca)}
        render(:path=>'iv', :template_class=>c, :template_block=>proca, :cache=>true)
      end
    end

    body('/a').strip.must_equal "iv-a"
    app.render_opts[:cache][['iv', c, nil, nil, proca]].must_be_nil
    body('/b').strip.must_equal "iv-a"
    app.render_opts[:cache][['iv', c, nil, nil, proca]].wont_equal nil
  end

  it "Support :cache_key option to force the key used when caching" do
    app(:bare) do
      plugin :render, :views=>"./spec/views"

      route do |r|
        @a = 'a'
        r.is('a'){render('iv', :cache_key=>:a)}
        r.is('about'){render('about', :cache_key=>:a, :cache=>false, :locals=>{:title=>'a'})}
        render('about', :cache_key=>:a)
      end
    end

    body('/a').strip.must_equal "a"
    body('/b').strip.must_equal "a"
    body('/about').strip.must_equal "<h1>a</h1>"
  end

  it "Support :scope option to override object in which to evaluate the template" do
    app(:bare) do
      plugin :render, :views=>"./spec/views"

      route do |r|
        render(:inline=>'<%= first %>-<%= last %>', :scope=>[1,2])
      end
    end

    body.must_equal "1-2"
  end

  it "should dup render_opts when subclasses, including an empty cache" do
    c = Class.new(Roda)
    c.plugin :render
    c.render_opts[:cache][:foo] = 1
    sc = Class.new(c)

    c.render_opts.wont_be_same_as(sc.render_opts)
    c.render_opts[:cache].wont_be_same_as(sc.render_opts[:cache])
    sc.render_opts[:cache][:foo].must_be_nil
  end

  it "should use a copy of superclass's cache when inheriting if :inherit_cache option is used" do
    c = Class.new(Roda)
    c.plugin :render, :inherit_cache=>true
    c.render_opts[:cache][:foo] = 1
    sc = Class.new(c)

    c.render_opts.wont_be_same_as(sc.render_opts)
    c.render_opts[:cache].wont_be_same_as(sc.render_opts[:cache])
    sc.render_opts[:cache][:foo].must_equal 1
  end

  it "should not modifying existing cache if loading the plugin a separate time" do
    c = Class.new(Roda)
    c.plugin :render
    cache = c.render_opts[:cache]
    c.plugin :render
    c.render_opts[:cache].must_be_same_as cache

    c.plugin :render, :cache=>false
    c.render_opts[:cache].must_equal false
    c.plugin :render
    c.render_opts[:cache].must_equal false
  end

  it "render plugin call should not override existing options" do
    c = Class.new(Roda)
    c.plugin :render, :layout=>:foo
    c.plugin :render
    c.render_opts[:layout].must_equal :foo
  end

  it "should not use cache in subclass if caching disabled in superclass" do
    app(:bare) do
      plugin :render, :views=>"./spec/views", :cache=>false
      
      route do |r|
        view(:inline=>"Hello <%= name %>: <%= render_opts[:cache] %>", :locals=>{:name => "Agent Smith"}, :layout=>nil)
      end
    end

    body("/inline").strip.must_equal "Hello Agent Smith: false"

    Class.new(app).render_opts[:cache].must_equal false
  end

  it "with :check_paths=>true" do
    render_opts = {}
    app(:bare) do
      plugin :render, :views=>"./spec/views", :check_paths=>true
      
      route do |r|
        r.get 'a' do
          render("a", render_opts)
        end

        r.get 'c' do
          render("about/_test", :locals=>{:title=>'a'})
        end

        render("b", render_opts)
      end
    end

    body.strip.must_equal "b"
    req("/a")
    req("/c")

    app.plugin :render, :allowed_paths=>[]
    proc{req}.must_raise Roda::RodaError
    proc{req("/a")}.must_raise Roda::RodaError
    proc{req("/c")}.must_raise Roda::RodaError

    app.plugin :render, :allowed_paths=>['spec/views/about']
    proc{req}.must_raise Roda::RodaError
    proc{req("/a")}.must_raise Roda::RodaError
    req("/c")

    app.plugin :render, :allowed_paths=>%w'spec/views/about spec/views/b'
    body.strip.must_equal "b"
    proc{req("/a")}.must_raise Roda::RodaError
    req("/c")

    render_opts[:check_paths] = true
    app.plugin :render, :check_paths=>false
    body.strip.must_equal "b"
    proc{req("/a")}.must_raise Roda::RodaError
    req("/c")

    render_opts.delete(:check_paths)
    app.plugin :render
    body.strip.must_equal "b"
    req("/a")
    req("/c")

    render_opts[:check_paths] = true
    body.strip.must_equal "b"
    proc{req("/a")}.must_raise Roda::RodaError
    req("/c")
  end

  it "with a cache_class set" do
    app(:bare) do
      test_cache = Class.new(Roda::RodaCache) do
        def [](key)
          super
        end
        def []=(key, value)
          super
        end
        def test_method
        end
      end

      plugin :render, :views=>"./spec/views", :cache=>true, :cache_class=>test_cache

      route do |r|
        view(:inline=>"foo", :layout=>nil)
      end
    end

    body("/inline").strip.must_equal "foo"

    Class.new(app).render_opts[:cache].must_respond_to :test_method
  end

end
end

begin
  require 'tilt'
  require 'tilt/erubi'
rescue LoadError
  warn "tilt 2 or erubi not installed, skipping render :escape=>:erubi test"  
else
describe "_erubis_escaping plugin" do
  before do
    if defined?(Tilt::ErubiTemplate) && ::Tilt['erb'] != Tilt::ErubiTemplate
      # Set erubi as default erb template handler
      Tilt.register(Tilt::ErubiTemplate, 'erb')
    end
  end

  it "should escape inside <%= %> and not inside <%== %>, and handle postfix conditionals" do
    app(:bare) do
      plugin :render, :escape=>:erubi

      route do |r|
        render(:inline=>'<%= "<>" %> <%== "<>" %><%= "<>" if false %>')
      end
    end

    body.must_equal '&lt;&gt; <>'
  end

  it "should allow for per-branch escaping via set_view options" do
    app(:bare) do
      plugin :render, :escape=>:erubi
      plugin :view_options

      route do |r|
        set_view_options :template_opts=>{:escape=>false}
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
