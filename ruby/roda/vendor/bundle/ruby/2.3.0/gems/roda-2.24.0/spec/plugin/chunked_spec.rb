require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

begin
  require 'tilt/erb'
rescue LoadError
  warn "tilt not installed, skipping chunked plugin test"  
else
describe "chunked plugin" do 
  def cbody(env={})
    body({'HTTP_VERSION'=>'HTTP/1.1'}.merge(env))
  end

  it "streams templates in chunked encoding only if HTTP 1.1 is used" do
    app(:chunked) do |r|
      chunked(:inline=>'m', :layout=>{:inline=>'h<%= yield %>t'})
    end

    cbody.must_equal "1\r\nh\r\n1\r\nm\r\n1\r\nt\r\n0\r\n\r\n"
    body.must_equal "hmt"
  end

  it "hex encodes chunk sizes" do
    m = 'm' * 31
    app(:chunked) do |r|
      chunked(:inline=>m.dup, :layout=>{:inline=>'h<%= yield %>t'})
    end

    cbody.must_equal "1\r\nh\r\n1f\r\n#{m}\r\n1\r\nt\r\n0\r\n\r\n"
    body.must_equal "h#{m}t"
  end

  it "accepts a block that is called after layout yielding but before content when streaming" do
    app(:chunked) do |r|
      @h = nil
      chunked(:inline=>'m<%= @h %>', :layout=>{:inline=>'<%= @h %><%= yield %>t'}) do
        @h = 'h'
      end
    end

    cbody.must_equal "2\r\nmh\r\n1\r\nt\r\n0\r\n\r\n"
    body.must_equal "hmht"
  end

  it "has delay accept block that is called after layout yielding but before content when streaming" do
    app(:chunked) do |r|
      delay do
        @h << 'i'
      end
      @h = String.new('h')
      chunked(:inline=>'m<%= @h %>', :layout=>{:inline=>'<%= @h %><%= yield %>t'}) do
        @h << 'j'
      end
    end

    cbody.must_equal "1\r\nh\r\n4\r\nmhij\r\n1\r\nt\r\n0\r\n\r\n"
    body.must_equal "hijmhijt"
  end

  it "has delay raise if not given a block" do
    app(:chunked){|r| delay}
    proc{body}.must_raise(Roda::RodaError)
  end

  it "works when a layout is not used" do
    app(:chunked) do |r|
      chunked(:inline=>'m', :layout=>nil)
    end

    cbody.must_equal "1\r\nm\r\n0\r\n\r\n"
    body.must_equal "m"
  end

  it "streams partial template responses if flush is used in content template" do
    app(:chunked) do |r|
      chunked(:inline=>'m<%= flush %>n', :layout=>{:inline=>'h<%= yield %>t'})
    end

    cbody.must_equal "1\r\nh\r\n1\r\nm\r\n1\r\nn\r\n1\r\nt\r\n0\r\n\r\n"
    body.must_equal "hmnt"
  end

  it "streams partial template responses if flush is used in layout template" do
    app(:chunked) do |r|
      chunked(:inline=>'m', :layout=>{:inline=>'h<%= flush %>i<%= yield %>t'})
    end

    cbody.must_equal "1\r\nh\r\n1\r\ni\r\n1\r\nm\r\n1\r\nt\r\n0\r\n\r\n"
    body.must_equal "himt"
  end

  it "does not stream if no_chunk! is used" do
    app(:chunked) do |r|
      no_chunk!
      chunked(:inline=>'m', :layout=>{:inline=>'h<%= yield %>t'})
    end

    cbody.must_equal "hmt"
    body.must_equal "hmt"
  end

  it "streams existing response body before call" do
    app(:chunked) do |r|
      response.write('a')
      response.write chunked(:inline=>'m', :layout=>{:inline=>'h<%= yield %>t'})
    end

    cbody.must_equal "1\r\na\r\n1\r\nh\r\n1\r\nm\r\n1\r\nt\r\n0\r\n\r\n"
    body.must_equal "ahmt"
  end

  it "should not include Content-Length header even if body is already written to" do
    app(:chunked) do |r|
      response.write('a')
      response.write chunked(:inline=>'m', :layout=>{:inline=>'h<%= yield %>t'})
    end

    header('Content-Length', 'HTTP_VERSION'=>'HTTP/1.1').must_be_nil
    header('Content-Length', 'HTTP_VERSION'=>'HTTP/1.0').must_equal '4'
  end

  it "stream template responses for view if :chunk_by_default is used" do
    app(:bare) do
      plugin :chunked, :chunk_by_default=>true
      route do |r|
        view(:inline=>'m', :layout=>{:inline=>'h<%= yield %>t'})
      end
    end

    cbody.must_equal "1\r\nh\r\n1\r\nm\r\n1\r\nt\r\n0\r\n\r\n"
    body.must_equal "hmt"
  end

  it "uses Transfer-Encoding header when chunking" do
    app(:chunked) do |r|
      chunked(:inline=>'m', :layout=>{:inline=>'h<%= yield %>t'})
    end

    header('Transfer-Encoding', 'HTTP_VERSION'=>'HTTP/1.1').must_equal 'chunked'
    header('Transfer-Encoding', 'HTTP_VERSION'=>'HTTP/1.0').must_be_nil
  end

  it "uses given :headers when chunking" do
    app(:bare) do
      plugin :chunked, :headers=>{'Foo'=>'bar'}
      route do |r|
        chunked(:inline=>'m', :layout=>{:inline=>'h<%= yield %>t'})
      end
    end

    header('Foo', 'HTTP_VERSION'=>'HTTP/1.1').must_equal 'bar'
    header('Foo', 'HTTP_VERSION'=>'HTTP/1.0').must_be_nil
  end

  it "handles multiple arguments to chunked" do
    app(:bare) do
      plugin :chunked, :chunk_by_default=>true
      plugin :render, :views => "./spec/views"
      route do |r|
        chunked('about', :locals=>{:title=>'m'}, :layout=>{:inline=>'h<%= yield %>t'})
      end
    end

    cbody.must_equal "1\r\nh\r\nb\r\n<h1>m</h1>\n\r\n1\r\nt\r\n0\r\n\r\n"
    body.must_equal "h<h1>m</h1>\nt"
  end

  it "handles multiple hash arguments to chunked" do
    app(:chunked) do |r|
      chunked({:inline=>'m'}, :layout=>{:inline=>'h<%= yield %>t'})
    end

    cbody.must_equal "1\r\nh\r\n1\r\nm\r\n1\r\nt\r\n0\r\n\r\n"
    body.must_equal "hmt"
  end

  it "handles :layout_opts option" do
    app(:chunked) do |r|
      chunked(:inline=>'m', :layout=>{:inline=>'<%= h %><%= yield %>t'}, :layout_opts=>{:locals=>{:h=>'h'}})
    end

    cbody.must_equal "1\r\nh\r\n1\r\nm\r\n1\r\nt\r\n0\r\n\r\n"
    body.must_equal "hmt"
  end

  it "uses handle_chunk_error for handling errors when chunking" do
    app(:chunked) do |r|
      chunked(:inline=>'m', :layout=>{:inline=>'h<%= yield %><% raise %>'})
    end
    proc{cbody}.must_raise RuntimeError
    proc{body}.must_raise RuntimeError

    app.send(:define_method, :handle_chunk_error) do |v|
      @_out_buf = 'e'
      flush
    end

    cbody.must_equal "1\r\nh\r\n1\r\nm\r\n1\r\ne\r\n0\r\n\r\n"
    proc{body}.must_raise RuntimeError
  end
end
end
