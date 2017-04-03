require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "public plugin" do 
  it "adds r.public for serving static files from public folder" do
    app(:bare) do
      plugin :public, :root=>'spec/views'

      route do |r|
        r.public

        r.on 'static' do
          r.public
        end
      end
    end

    status("/about/_test.erb\0").must_equal 404
    body('/about/_test.erb').must_equal File.read('spec/views/about/_test.erb')
    body('/static/about/_test.erb').must_equal File.read('spec/views/about/_test.erb')
    body('/foo/.././/about/_test.erb').must_equal File.read('spec/views/about/_test.erb')
  end

  it "respects the application's :root option" do
    app(:bare) do
      opts[:root] = File.expand_path('../../', __FILE__)
      plugin :public, :root=>'views'

      route do |r|
        r.public
      end
    end

    body('/about/_test.erb').must_equal File.read('spec/views/about/_test.erb')
  end

  it "handles serving gzip files in gzip mode if client supports gzip" do
    app(:bare) do
      plugin :public, :root=>'spec/views', :gzip=>true

      route do |r|
        r.public
      end
    end

    body('/about/_test.erb').must_equal File.read('spec/views/about/_test.erb')
    header('Content-Encoding', '/about/_test.erb').must_be_nil

    body('/about.erb').must_equal File.read('spec/views/about.erb')
    header('Content-Encoding', '/about.erb').must_be_nil

    meth = RUBY_VERSION >= '1.9' ? :binread : :read
    body('/about/_test.erb', 'HTTP_ACCEPT_ENCODING'=>'deflate, gzip').must_equal File.send(meth, 'spec/views/about/_test.erb.gz')
    h = req('/about/_test.erb', 'HTTP_ACCEPT_ENCODING'=>'deflate, gzip')[1]
    h['Content-Encoding'].must_equal 'gzip'
    h['Content-Type'].must_equal 'text/plain'

    body('/about/_test.css', 'HTTP_ACCEPT_ENCODING'=>'deflate, gzip').must_equal File.send(meth, 'spec/views/about/_test.css.gz')
    h = req('/about/_test.css', 'HTTP_ACCEPT_ENCODING'=>'deflate, gzip')[1]
    h['Content-Encoding'].must_equal 'gzip'
    h['Content-Type'].must_equal 'text/css'
  end
end
