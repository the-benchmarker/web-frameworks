require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))
require 'fileutils'

run_tests = true
begin
  begin
    require 'tilt/sass'
  rescue LoadError
    begin
      for lib in %w'tilt sass'
        require lib
      end
    rescue LoadError
      warn "#{lib} not installed, skipping assets plugin test"
      run_tests = false
    end
  end
end

if run_tests
  metadata_file = File.expand_path('spec/assets/tmp/precompiled.json')
  js_file = File.expand_path('spec/assets/js/head/app.js')
  css_file = File.expand_path('spec/assets/css/no_access.css')
  js_mtime = File.mtime(js_file)
  js_atime = File.atime(js_file)
  css_mtime = File.mtime(css_file)
  css_atime = File.atime(css_file)
  describe 'assets plugin' do
    before do
      app(:bare) do
        plugin :assets,
          :css => ['app.scss', 'raw.css'],
          :js => { :head => ['app.js'] },
          :path => 'spec/assets',
          :public => 'spec',
          :css_opts => {:cache=>false},
          :css_compressor => :none,
          :js_compressor => :none

        route do |r|
          r.assets

          r.is 'test' do
            "#{assets(:css)}\n#{assets([:js, :head])}"
          end

          r.is 'paths_test' do
            css_paths = assets_paths(:css)
            js_paths = assets_paths([:js, :head])
            empty_paths = assets_paths(:empty)
            { 'css' => css_paths, 'js' => js_paths, 'empty' => empty_paths }.map do |k, a|
            "#{k}:#{a.class}:#{a.length}:#{a.join(',')}"
            end.join("\n")
          end
        end
      end
    end
    after do
      File.utime(js_atime, js_mtime, js_file)
      File.utime(css_atime, css_mtime, css_file)
      File.delete(metadata_file) if File.file?(metadata_file)
    end
    after(:all) do
      FileUtils.rm_r('spec/assets/tmp') if File.directory?('spec/assets/tmp')
      FileUtils.rm_r('spec/public') if File.directory?('spec/public')
      FileUtils.rm(Dir['spec/assets/app.*.{js,css}*'])
    end

    def gunzip(body)
      Zlib::GzipReader.wrap(StringIO.new(body), &:read)
    end

    it 'assets_opts should use correct paths given options' do
      fpaths = [:js_path, :css_path, :compiled_js_path, :compiled_css_path]
      rpaths = [:js_prefix, :css_prefix, :compiled_js_prefix, :compiled_css_prefix]
      app.assets_opts.values_at(*fpaths).must_equal %w"spec/assets/js/ spec/assets/css/ spec/assets/app spec/assets/app".map{|s| File.join(Dir.pwd, s)}
      app.assets_opts.values_at(*rpaths).must_equal %w"assets/js/ assets/css/ assets/app assets/app"

      app.plugin :assets, :path=>'bar/', :public=>'foo/', :prefix=>'as/', :js_dir=>'j/', :css_dir=>'c/', :compiled_name=>'a'
      app.assets_opts.values_at(*fpaths).must_equal %w"bar/j/ bar/c/ foo/as/a foo/as/a".map{|s| File.join(Dir.pwd, s)}
      app.assets_opts.values_at(*rpaths).must_equal %w"as/j/ as/c/ as/a as/a"

      app.plugin :assets, :path=>'bar', :public=>'foo', :prefix=>'as', :js_dir=>'j', :css_dir=>'c', :compiled_name=>'a'
      app.assets_opts.values_at(*fpaths).must_equal %w"bar/j/ bar/c/ foo/as/a foo/as/a".map{|s| File.join(Dir.pwd, s)}
      app.assets_opts.values_at(*rpaths).must_equal %w"as/j/ as/c/ as/a as/a"

      app.plugin :assets, :compiled_js_dir=>'cj', :compiled_css_dir=>'cs', :compiled_path=>'cp'
      app.assets_opts.values_at(*fpaths).must_equal %w"bar/j/ bar/c/ foo/cp/cj/a foo/cp/cs/a".map{|s| File.join(Dir.pwd, s)}
      app.assets_opts.values_at(*rpaths).must_equal %w"as/j/ as/c/ as/cj/a as/cs/a"

      app.plugin :assets, :compiled_js_route=>'cjr', :compiled_css_route=>'ccr', :js_route=>'jr', :css_route=>'cr'
      app.assets_opts.values_at(*fpaths).must_equal %w"bar/j/ bar/c/ foo/cp/cj/a foo/cp/cs/a".map{|s| File.join(Dir.pwd, s)}
      app.assets_opts.values_at(*rpaths).must_equal %w"as/jr/ as/cr/ as/cjr/a as/ccr/a"

      app.plugin :assets, :compiled_js_route=>'cj', :compiled_css_route=>'cs', :js_route=>'j', :css_route=>'c'
      app.assets_opts.values_at(*fpaths).must_equal %w"bar/j/ bar/c/ foo/cp/cj/a foo/cp/cs/a".map{|s| File.join(Dir.pwd, s)}
      app.assets_opts.values_at(*rpaths).must_equal %w"as/j/ as/c/ as/cj/a as/cs/a"

      app.plugin :assets
      app.assets_opts.values_at(*fpaths).must_equal %w"bar/j/ bar/c/ foo/cp/cj/a foo/cp/cs/a".map{|s| File.join(Dir.pwd, s)}
      app.assets_opts.values_at(*rpaths).must_equal %w"as/j/ as/c/ as/cj/a as/cs/a"

      app.plugin :assets, :compiled_js_dir=>'', :compiled_css_dir=>nil, :compiled_js_route=>nil, :compiled_css_route=>nil
      app.assets_opts.values_at(*fpaths).must_equal %w"bar/j/ bar/c/ foo/cp/a foo/cp/a".map{|s| File.join(Dir.pwd, s)}
      app.assets_opts.values_at(*rpaths).must_equal %w"as/j/ as/c/ as/a as/a"

      app.plugin :assets, :js_dir=>'', :css_dir=>nil, :js_route=>nil, :css_route=>nil
      app.assets_opts.values_at(*fpaths).must_equal %w"bar/ bar/ foo/cp/a foo/cp/a".map{|s| File.join(Dir.pwd, s)}
      app.assets_opts.values_at(*rpaths).must_equal %w"as/ as/ as/a as/a"

      app.plugin :assets, :public=>''
      app.assets_opts.values_at(*fpaths).must_equal %w"bar/ bar/ cp/a cp/a".map{|s| File.join(Dir.pwd, s)}
      app.assets_opts.values_at(*rpaths).must_equal %w"as/ as/ as/a as/a"

      app.plugin :assets, :path=>'', :compiled_path=>nil
      app.assets_opts.values_at(*fpaths).must_equal ['', '', 'a', 'a'].map{|s| File.join(Dir.pwd, s)}
      app.assets_opts.values_at(*rpaths).must_equal ['as/', 'as/', 'as/a', 'as/a']

      app.plugin :assets, :prefix=>''
      app.assets_opts.values_at(*fpaths).must_equal ['', '', 'a', 'a'].map{|s| File.join(Dir.pwd, s)}
      app.assets_opts.values_at(*rpaths).must_equal ['', '', 'a', 'a']

      app.plugin :assets, :compiled_name=>nil
      app.assets_opts.values_at(*fpaths).must_equal ['', ''].map{|s| File.join(Dir.pwd, s)} + ['', ''].map{|s| File.join(Dir.pwd, s).chop}
      app.assets_opts.values_at(*rpaths).must_equal ['', '', '', '']
    end

    it 'assets_opts should use headers and dependencies given options' do
      keys = [:css_headers, :js_headers, :dependencies]
      app.assets_opts.values_at(*keys).must_equal [{'Content-Type'=>"text/css; charset=UTF-8"}, {'Content-Type'=>"application/javascript; charset=UTF-8"}, {}]

      app.plugin :assets, :headers=>{'A'=>'B'}, :dependencies=>{'a'=>'b'}
      app.assets_opts.values_at(*keys).must_equal [{'Content-Type'=>"text/css; charset=UTF-8", 'A'=>'B'}, {'Content-Type'=>"application/javascript; charset=UTF-8", 'A'=>'B'}, {'a'=>'b'}]

      app.plugin :assets, :css_headers=>{'C'=>'D'}, :js_headers=>{'E'=>'F'}, :dependencies=>{'c'=>'d'}
      app.assets_opts.values_at(*keys).must_equal [{'Content-Type'=>"text/css; charset=UTF-8", 'A'=>'B', 'C'=>'D'}, {'Content-Type'=>"application/javascript; charset=UTF-8", 'A'=>'B', 'E'=>'F'}, {'a'=>'b', 'c'=>'d'}]

      app.plugin :assets
      app.assets_opts.values_at(*keys).must_equal [{'Content-Type'=>"text/css; charset=UTF-8", 'A'=>'B', 'C'=>'D'}, {'Content-Type'=>"application/javascript; charset=UTF-8", 'A'=>'B', 'E'=>'F'}, {'a'=>'b', 'c'=>'d'}]

      app.plugin :assets
      app.assets_opts.values_at(*keys).must_equal [{'Content-Type'=>"text/css; charset=UTF-8", 'A'=>'B', 'C'=>'D'}, {'Content-Type'=>"application/javascript; charset=UTF-8", 'A'=>'B', 'E'=>'F'}, {'a'=>'b', 'c'=>'d'}]

      app.plugin :assets, :headers=>{'Content-Type'=>'C', 'E'=>'G'}
      app.assets_opts.values_at(*keys).must_equal [{'Content-Type'=>"C", 'A'=>'B', 'C'=>'D', 'E'=>'G'}, {'Content-Type'=>"C", 'A'=>'B', 'E'=>'F'}, {'a'=>'b', 'c'=>'d'}]

      app.plugin :assets, :css_headers=>{'A'=>'B1'}, :js_headers=>{'E'=>'F1'}, :dependencies=>{'c'=>'d1'}
      app.assets_opts.values_at(*keys).must_equal [{'Content-Type'=>"C", 'A'=>'B1', 'C'=>'D', 'E'=>'G'}, {'Content-Type'=>"C", 'A'=>'B', 'E'=>'F1'}, {'a'=>'b', 'c'=>'d1'}]
    end

    it 'assets_paths should return arrays of source paths' do
      html = body('/paths_test')
      html.scan('css:Array:2:/assets/css/app.scss,/assets/css/raw.css').length.must_equal 1
      html.scan('js:Array:1:/assets/js/head/app.js').length.must_equal 1
      html.scan('empty:Array:0').length.must_equal 1
    end

    it 'assets_paths should return the compiled path in an array' do
      app.compile_assets
      html = body('/paths_test')
      css_hash = app.assets_opts[:compiled]['css']
      js_hash = app.assets_opts[:compiled]['js.head']
      html.scan("css:Array:1:/assets/app.#{css_hash}.css").length.must_equal 1
      html.scan("js:Array:1:/assets/app.head.#{js_hash}.js").length.must_equal 1
      html.scan('empty:Array:0').length.must_equal 1
    end

    it 'should handle rendering assets, linking to them, and accepting requests for them when not compiling' do
      html = body('/test')
      html.scan(/<link/).length.must_equal 2
      html =~ %r{href="(/assets/css/app\.scss)"}
      css = body($1)
      html =~ %r{href="(/assets/css/raw\.css)"}
      css2 = body($1)
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/assets/js/head/app\.js)"}
      js = body($1)
      css.must_match(/color: red;/)
      css2.must_match(/color: blue;/)
      js.must_include('console.log')
    end

    it 'should handle rendering assets, linking to them, and accepting requests for them  when :add_script_name app option is used' do
      app.opts[:add_script_name] = true
      app.plugin :assets
      html = body('/test', 'SCRIPT_NAME'=>'/foo')
      html.scan(/<link/).length.must_equal 2
      html =~ %r{href="/foo(/assets/css/app\.scss)"}
      css = body($1)
      html =~ %r{href="/foo(/assets/css/raw\.css)"}
      css2 = body($1)
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="/foo(/assets/js/head/app\.js)"}
      js = body($1)
      css.must_match(/color: red;/)
      css2.must_match(/color: blue;/)
      js.must_include('console.log')
    end

    it 'should handle rendering assets, linking to them, and accepting requests for them when not compiling, with different options' do
      app.plugin :assets, :path=>'spec/', :js_dir=>'assets/js', :css_dir=>'assets/css', :prefix=>'a',
        :js_route=>'foo', :css_route=>'bar', :add_suffix=>true, :css_opts=>{:style=>:compressed}
      html = body('/test')
      html.scan(/<link/).length.must_equal 2
      html =~ %r{href="(/a/bar/app\.scss.css)"}
      css = body($1)
      html =~ %r{href="(/a/bar/raw\.css.css)"}
      css2 = body($1)
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/a/foo/head/app\.js.js)"}
      js = body($1)
      css.must_match(/color:red\}/)
      css2.must_match(/color: blue;/)
      js.must_include('console.log')
    end

    it 'should handle rendering assets, linking to them, and accepting requests for them when not compiling with a multi-level hash' do
      app.plugin :assets, :path=>'spec', :js_dir=>nil, :css_dir=>nil, :prefix=>nil,
        :css=>{:assets=>{:css=>%w'app.scss raw.css'}}, :js=>{:assets=>{:js=>{:head=>'app.js'}}}
      app.route do |r|
        r.assets
        r.is 'test' do
          "#{assets([:css, :assets, :css])}\n#{assets([:js, :assets, :js, :head])}"
        end
      end
      html = body('/test')
      html.scan(/<link/).length.must_equal 2
      html =~ %r{href="(/assets/css/app\.scss)"}
      css = body($1)
      html =~ %r{href="(/assets/css/raw\.css)"}
      css2 = body($1)
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/assets/js/head/app\.js)"}
      js = body($1)
      css.must_match(/color: red;/)
      css2.must_match(/color: blue;/)
      js.must_include('console.log')
    end

    it 'should handle :group_subdirs => false' do
      app.plugin :assets, :path=>'spec', :js_dir=>nil, :css_dir=>nil, :prefix=>nil, :group_subdirs=>false,
        :css=>{:assets=>{:css=>%w'assets/css/app.scss assets/css/raw.css'}}, :js=>{:assets=>{:js=>{:head=>'assets/js/head/app.js'}}}
      app.route do |r|
        r.assets
        r.is 'test' do
          "#{assets([:css, :assets, :css])}\n#{assets([:js, :assets, :js, :head])}"
        end
      end
      html = body('/test')
      html.scan(/<link/).length.must_equal 2
      html =~ %r{href="(/assets/css/app\.scss)"}
      css = body($1)
      html =~ %r{href="(/assets/css/raw\.css)"}
      css2 = body($1)
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/assets/js/head/app\.js)"}
      js = body($1)
      css.must_match(/color: red;/)
      css2.must_match(/color: blue;/)
      js.must_include('console.log')
    end

    it 'should handle compressing using different libraries' do
      try_compressor = proc do |css, js|
        app.plugin :assets, :css_compressor=>css, :js_compressor=>js
        begin
          app.compile_assets
        rescue LoadError, Roda::RodaPlugins::Assets::CompressorNotFound
          next
        end
        File.read("spec/assets/app.#{app.assets_opts[:compiled]['css']}.css").must_match(/color: ?blue/)
        File.read("spec/assets/app.head.#{app.assets_opts[:compiled]['js.head']}.js").must_include('console.log')
      end

      try_compressor.call(nil, nil)
      try_compressor.call(:yui, :yui)
      try_compressor.call(:none, :closure)
      try_compressor.call(:none, :uglifier)
      try_compressor.call(:none, :minjs)
    end

    it 'should handle compiling assets, linking to them, and accepting requests for them' do
      app.compile_assets
      html = body('/test')
      html.scan(/<link/).length.must_equal 1
      html =~ %r{href="(/assets/app\.[a-f0-9]{40}\.css)"}
      css = body($1)
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/assets/app\.head\.[a-f0-9]{40}\.js)"}
      js = body($1)
      css.must_match(/color: ?red/)
      css.must_match(/color: ?blue/)
      js.must_include('console.log')
    end

    [[:sha256, 64, 44], [:sha384, 96, 64], [:sha512, 128, 88]].each do |algo, hex_length, base64_length|
      it "should handle :sri option for subresource integrity for #{algo} when compiling assets" do
        app.plugin :assets, :sri=>algo
        app.compile_assets
        html = body('/test')
        html.scan(/<link/).length.must_equal 1
        html =~ %r|et" integrity="#{algo}-([^"]+)"|
        css_hash = $1
        css_hash.length.must_equal base64_length
        html =~ %r|href="(/assets/app\.[a-f0-9]{#{hex_length}}\.css)"|
        css = body($1)
        [Digest.const_get(algo.to_s.upcase).digest(css)].pack('m').tr("\n", "").must_equal css_hash
        html.scan(/<script/).length.must_equal 1
        html =~ %r|pt" integrity="#{algo}-([^"]+)"|
        js_hash = $1
        js_hash.length.must_equal base64_length
        html =~ %r|src="(/assets/app\.head\.[a-f0-9]{#{hex_length}}\.js)"|
        js = body($1)
        [Digest.const_get(algo.to_s.upcase).digest(js)].pack('m').tr("\n", "").must_equal js_hash
        css.must_match(/color: ?red/)
        css.must_match(/color: ?blue/)
        js.must_include('console.log')
      end
    end

    it 'should handle linking to compiled assets when a compiled asset host is used' do
      app.plugin :assets, :compiled_asset_host=>'https://cdn.example.com'
      app.compile_assets
      html = body('/test')
      html.scan(/<link/).length.must_equal 1
      html.must_match %r{href="https://cdn\.example\.com/assets/app\.[a-f0-9]{40}\.css"}
      html.scan(/<script/).length.must_equal 1
      html.must_match %r{src="https://cdn\.example\.com/assets/app\.head\.[a-f0-9]{40}\.js"}
    end

    it 'should handle compiling assets, linking to them, and accepting requests for them when :gzip is set' do
      app.plugin :assets, :gzip=>true
      app.compile_assets
      html = body('/test')
      html.scan(/<link/).length.must_equal 1
      html =~ %r{href="(/assets/app\.[a-f0-9]{40}\.css)"}
      css_path = $1
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/assets/app\.head\.[a-f0-9]{40}\.js)"}
      js_path = $1

      css = body(css_path)
      js = body(js_path)
      css.must_match(/color: ?red/)
      css.must_match(/color: ?blue/)
      js.must_include('console.log')

      css = gunzip(body(css_path, 'HTTP_ACCEPT_ENCODING'=>'deflate, gzip'))
      js = gunzip(body(js_path, 'HTTP_ACCEPT_ENCODING'=>'deflate, gzip'))
      css.must_match(/color: ?red/)
      css.must_match(/color: ?blue/)
      js.must_include('console.log')
    end

    it 'should handle compiling assets, linking to them, and accepting requests for them when :add_script_name app option is used' do
      app.opts[:add_script_name] = true
      app.plugin :assets
      app.compile_assets
      html = body('/test', 'SCRIPT_NAME'=>'/foo')
      html =~ %r{href="/foo(/assets/app\.[a-f0-9]{40}\.css)"}
      css = body($1)
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="/foo(/assets/app\.head\.[a-f0-9]{40}\.js)"}
      js = body($1)
      css.must_match(/color: ?red/)
      css.must_match(/color: ?blue/)
      js.must_include('console.log')
    end

    it 'should handle compiling assets, linking to them, and accepting requests for them, with different options' do
      app.plugin :assets, :compiled_path=>nil, :js_dir=>'assets/js', :css_dir=>'assets/css', :prefix=>'a',
        :public=>'spec/assets', :path=>'spec', :compiled_js_route=>'foo', :compiled_css_route=>'bar'
      app.compile_assets
      html = body('/test')
      html.scan(/<link/).length.must_equal 1
      html =~ %r{href="(/a/bar/app\.[a-f0-9]{40}\.css)"}
      css = body($1)
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/a/foo/app\.head\.[a-f0-9]{40}\.js)"}
      js = body($1)
      css.must_match(/color: ?red/)
      css.must_match(/color: ?blue/)
      js.must_include('console.log')
    end

    it 'should handle rendering assets, linking to them, and accepting requests for them when not compiling with a multi-level hash' do
      app.plugin :assets, :path=>'spec', :js_dir=>nil, :css_dir=>nil, :compiled_js_dir=>nil, :compiled_css_dir=>nil,
        :css=>{:assets=>{:css=>%w'app.scss raw.css'}}, :js=>{:assets=>{:js=>{:head=>'app.js'}}}
      app.compile_assets
      app.route do |r|
        r.assets
        r.is 'test' do
          "#{assets([:css, :assets, :css])}\n#{assets([:js, :assets, :js, :head])}"
        end
      end
      html = body('/test')
      html.scan(/<link/).length.must_equal 1
      html =~ %r{href="(/assets/app\.assets\.css\.[a-f0-9]{40}\.css)"}
      css = body($1)
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/assets/app\.assets\.js\.head\.[a-f0-9]{40}\.js)"}
      js = body($1)
      css.must_match(/color: ?red/)
      css.must_match(/color: ?blue/)
      js.must_include('console.log')
    end

    it 'should handle rendering assets, linking to them, and accepting requests for them when not compiling with a multi-level hash when :add_script_name app option is used' do
      app.opts[:add_script_name] = true
      app.plugin :assets, :path=>'spec', :js_dir=>nil, :css_dir=>nil, :compiled_js_dir=>nil, :compiled_css_dir=>nil,
        :css=>{:assets=>{:css=>%w'app.scss raw.css'}}, :js=>{:assets=>{:js=>{:head=>'app.js'}}}
      app.compile_assets
      app.route do |r|
        r.assets
        r.is 'test' do
          "#{assets([:css, :assets, :css])}\n#{assets([:js, :assets, :js, :head])}"
        end
      end
      html = body('/test', 'SCRIPT_NAME'=>'/foo')
      html.scan(/<link/).length.must_equal 1
      html =~ %r{href="/foo(/assets/app\.assets\.css\.[a-f0-9]{40}\.css)"}
      css = body($1)
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="/foo(/assets/app\.assets\.js\.head\.[a-f0-9]{40}\.js)"}
      js = body($1)
      css.must_match(/color: ?red/)
      css.must_match(/color: ?blue/)
      js.must_include('console.log')
    end

    it 'should handle :group_subdirs => false when compiling' do
      app.plugin :assets, :path=>'spec', :js_dir=>nil, :css_dir=>nil, :compiled_js_dir=>nil, :compiled_css_dir=>nil, :group_subdirs=>false,
        :css=>{:assets=>{:css=>%w'assets/css/app.scss assets/css/raw.css'}}, :js=>{:assets=>{:js=>{:head=>'assets/js/head/app.js'}}}
      app.compile_assets
      app.route do |r|
        r.assets
        r.is 'test' do
          "#{assets([:css, :assets, :css])}\n#{assets([:js, :assets, :js, :head])}"
        end
      end
      html = body('/test')
      html.scan(/<link/).length.must_equal 1
      html =~ %r{href="(/assets/app\.assets\.css\.[a-f0-9]{40}\.css)"}
      css = body($1)
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/assets/app\.assets\.js\.head\.[a-f0-9]{40}\.js)"}
      js = body($1)
      css.must_match(/color: ?red/)
      css.must_match(/color: ?blue/)
      js.must_include('console.log')
    end

    it 'should handle automatically creating directories when compiling assets' do
      app.plugin :assets, :public=>'spec/public'
      app.compile_assets
      html = body('/test')
      html.scan(/<link/).length.must_equal 1
      html =~ %r{href="(/assets/app\.[a-f0-9]{40}\.css)"}
      css = body($1)
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/assets/app\.head\.[a-f0-9]{40}\.js)"}
      js = body($1)
      css.must_match(/color: ?red/)
      css.must_match(/color: ?blue/)
      js.must_include('console.log')
    end

    it 'should handle compiling only css assets' do
      app.compile_assets(:css)
      html = body('/test')
      html.scan(/<link/).length.must_equal 1
      html =~ %r{href="(/assets/app\.[a-f0-9]{40}\.css)"}
      css = body($1)
      html.scan(/<script/).length.must_equal 0
      css.must_match(/color: ?red/)
      css.must_match(/color: ?blue/)
    end

    it 'should handle compiling only js assets' do
      app.compile_assets(:js)
      html = body('/test')
      html.scan(/<link/).length.must_equal 0
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/assets/app\.head\.[a-f0-9]{40}\.js)"}
      js = body($1)
      js.must_include('console.log')
    end

    it 'should handle compiling asset subfolders' do
      app.compile_assets([:js, :head])
      html = body('/test')
      html.scan(/<link/).length.must_equal 0
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/assets/app\.head\.[a-f0-9]{40}\.js)"}
      js = body($1)
      js.must_include('console.log')
    end

    it 'should handle compiling assets when only a single asset type is present' do
      app.plugin :assets, :css=>nil
      app.compile_assets
      html = body('/test')
      html.scan(/<link/).length.must_equal 0
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/assets/app\.head\.[a-f0-9]{40}\.js)"}
      js = body($1)
      js.must_include('console.log')
    end

    it 'should handle compiling assets when an empty array is used' do
      app.plugin :assets, :css=>[]
      app.compile_assets
      html = body('/test')
      html.scan(/<link/).length.must_equal 0
      html.scan(/<script/).length.must_equal 1
      html =~ %r{src="(/assets/app\.head\.[a-f0-9]{40}\.js)"}
      js = body($1)
      js.must_include('console.log')
    end

    it '#assets must_include attributes given' do
      app.allocate.assets([:js, :head], 'a'=>'b').must_equal '<script type="text/javascript" a="b" src="/assets/js/head/app.js"></script>'
    end

    it '#assets should escape attribute values given' do
      app.allocate.assets([:js, :head], 'a'=>'b"e').must_equal '<script type="text/javascript" a="b&quot;e" src="/assets/js/head/app.js"></script>'
    end

    it 'requests for assets should return 304 if the asset has not been modified' do
      loc = '/assets/js/head/app.js'
      lm = header('Last-Modified', loc)
      status(loc, 'HTTP_IF_MODIFIED_SINCE'=>lm).must_equal 304
      body(loc, 'HTTP_IF_MODIFIED_SINCE'=>lm).must_equal ''
    end

    it 'requests for assets should not return 304 if the asset has been modified' do
      loc = '/assets/js/head/app.js'
      lm = header('Last-Modified', loc)
      File.utime(js_atime, js_mtime+1, js_file)
      status(loc, 'HTTP_IF_MODIFIED_SINCE'=>lm).must_equal 200
      body(loc, 'HTTP_IF_MODIFIED_SINCE'=>lm).must_include('console.log')
    end

    it 'requests for assets should return 304 if the dependency of an asset has not been modified' do
      app.plugin :assets, :dependencies=>{js_file=>css_file}
      loc = '/assets/js/head/app.js'
      lm = header('Last-Modified', loc)
      status(loc, 'HTTP_IF_MODIFIED_SINCE'=>lm).must_equal 304
      body(loc, 'HTTP_IF_MODIFIED_SINCE'=>lm).must_equal ''
    end

    it 'requests for assets should return 200 if the dependency of an asset has been modified' do
      app.plugin :assets, :dependencies=>{js_file=>css_file}
      loc = '/assets/js/head/app.js'
      lm = header('Last-Modified', loc)
      File.utime(css_atime, [css_mtime+2, js_mtime+2].max, css_file)
      status(loc, 'HTTP_IF_MODIFIED_SINCE'=>lm).must_equal 200
      body(loc, 'HTTP_IF_MODIFIED_SINCE'=>lm).must_include('console.log')
    end

    it 'should do a terminal match for assets' do
      status('/assets/css/app.scss/foo').must_equal 404
    end

    it 'should only allow files that you specify' do
      status('/assets/css/no_access.css').must_equal 404
    end

    it 'should not add routes for empty asset types' do
      app.plugin :assets, :css=>nil
      a = app::RodaRequest.assets_matchers
      a.length.must_equal 1
      a.first.length.must_equal 2
      a.first.first.must_equal :js
      'assets/js/head/app.js'.must_match a.first.last
      'assets/js/head/app2.js'.wont_match a.first.last
    end

    it 'should not add routes if no asset types' do
      app.plugin :assets, :js=>nil, :css=>nil
      app::RodaRequest.assets_matchers.must_equal []
    end

    it 'should support :postprocessor option' do
      postprocessor = lambda do |file, type, content|
        "file=#{file} type=#{type} tc=#{type.class} #{content.sub('color', 'font')}"
      end

      app.plugin :assets, :path=>'spec', :js_dir=>nil, :css_dir=>nil, :prefix=>nil,
        :postprocessor=>postprocessor,
        :css=>{:assets=>{:css=>%w'app.scss'}}
      app.route do |r|
        r.assets
        r.is 'test' do
          "#{assets([:css, :assets, :css])}"
        end
      end
      html = body('/test')
      html.scan(/<link/).length.must_equal 1
      html =~ %r{href="(/assets/css/app\.scss)"}
      css = body($1)
      css.must_match(/app\.scss/)
      css.must_match(/type=css/)
      css.must_match(/tc=Symbol/)
      css.must_match(/font: red;/)
    end

    it 'should support :precompiled option' do
      app.plugin :assets, :precompiled=>metadata_file
      File.exist?(metadata_file).must_equal false
      app.allocate.assets([:js, :head]).must_equal '<script type="text/javascript"  src="/assets/js/head/app.js"></script>'

      app.compile_assets
      File.exist?(metadata_file).must_equal true
      app.allocate.assets([:js, :head]).must_match %r{src="(/assets/app\.head\.[a-f0-9]{40}\.js)"}

      app.plugin :assets, :compiled=>false, :precompiled=>false
      app.allocate.assets([:js, :head]).must_equal '<script type="text/javascript"  src="/assets/js/head/app.js"></script>'

      app.plugin :assets, :precompiled=>metadata_file
      app.allocate.assets([:js, :head]).must_match %r{src="(/assets/app\.head\.[a-f0-9]{40}\.js)"}
    end

    it 'should work correctly with json plugin when r.assets is the last method called' do
      app.plugin :assets
      app.plugin :json
      app.route do |r|
        r.assets
      end
      status.must_equal 404
    end
  end

  describe 'assets plugin' do
    it "app :root option affects :views default" do
      app.plugin :assets
      app.assets_opts[:path].must_equal File.join(Dir.pwd, 'assets')
      app.assets_opts[:js_path].must_equal File.join(Dir.pwd, 'assets/js/')
      app.assets_opts[:css_path].must_equal File.join(Dir.pwd, 'assets/css/')

      app.opts[:root] = '/foo'
      app.plugin :assets
      # Work around for Windows
      app.assets_opts[:path].sub(/\A\w:/, '').must_equal '/foo/assets'
      app.assets_opts[:js_path].sub(/\A\w:/, '').must_equal '/foo/assets/js/'
      app.assets_opts[:css_path].sub(/\A\w:/, '').must_equal '/foo/assets/css/'

      app.opts[:root] = '/foo/bar'
      app.plugin :assets
      app.assets_opts[:path].sub(/\A\w:/, '').must_equal '/foo/bar/assets'
      app.assets_opts[:js_path].sub(/\A\w:/, '').must_equal '/foo/bar/assets/js/'
      app.assets_opts[:css_path].sub(/\A\w:/, '').must_equal '/foo/bar/assets/css/'

      app.opts[:root] = nil
      app.plugin :assets
      app.assets_opts[:path].must_equal File.join(Dir.pwd, 'assets')
      app.assets_opts[:js_path].must_equal File.join(Dir.pwd, 'assets/js/')
      app.assets_opts[:css_path].must_equal File.join(Dir.pwd, 'assets/css/')
    end
  end
end
