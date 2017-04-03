require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

require 'uri'

describe "sinatra_helpers plugin" do 
  def sin_app(&block)
    app(:sinatra_helpers, &block)
  end

  def status_app(code, &block)
    #code += 2 if [204, 205, 304].include? code
    block ||= proc{}
    sin_app do |r|
      status code
      instance_eval(&block).inspect
    end
  end

  it 'status returns the response status code if not given an argument' do
    status_app(207){status}
    body.must_equal "207"
  end

  it 'status sets the response status code if given an argument' do
    status_app 207
    status.must_equal 207
  end

  it 'not_found? is true only if status == 404' do
    status_app(404){not_found?}
    body.must_equal 'true'
    status_app(405){not_found?}
    body.must_equal 'false'
    status_app(403){not_found?}
    body.must_equal 'false'
  end

  it 'informational? is true only for 1xx status' do
    status_app(100 + rand(100)){informational?}
    body.must_equal 'true'
    status_app(200 + rand(400)){informational?}
    body.must_equal 'false'
  end

  it 'success? is true only for 2xx status' do
    status_app(200 + rand(100)){success?}
    body.must_equal 'true'
    status_app(100 + rand(100)){success?}
    body.must_equal 'false'
    status_app(300 + rand(300)){success?}
    body.must_equal 'false'
  end

  it 'redirect? is true only for 3xx status' do
    status_app(300 + rand(100)){redirect?}
    body.must_equal 'true'
    status_app(200 + rand(100)){redirect?}
    body.must_equal 'false'
    status_app(400 + rand(200)){redirect?}
    body.must_equal 'false'
  end

  it 'client_error? is true only for 4xx status' do
    status_app(400 + rand(100)){client_error?}
    body.must_equal 'true'
    status_app(200 + rand(200)){client_error?}
    body.must_equal 'false'
    status_app(500 + rand(100)){client_error?}
    body.must_equal 'false'
  end

  it 'server_error? is true only for 5xx status' do
    status_app(500 + rand(100)){server_error?}
    body.must_equal 'true'
    status_app(200 + rand(300)){server_error?}
    body.must_equal 'false'
  end

  describe 'body' do
    it 'takes a block for deferred body generation' do
      sin_app{body{'Hello World'}; nil}
      body.must_equal 'Hello World'
      header('Content-Length').must_equal '11'
    end

    it 'supports #join' do
      sin_app{body{'Hello World'}; nil}
      req[2].join.must_equal 'Hello World'
    end

    it 'takes a String, Array, or other object responding to #each' do
      sin_app{body 'Hello World'; nil}
      body.must_equal 'Hello World'
      header('Content-Length').must_equal '11'

      sin_app{body ['Hello ', 'World']; nil}
      body.must_equal 'Hello World'
      header('Content-Length').must_equal '11'

      o = Object.new
      def o.each; yield 'Hello World' end
      sin_app{body o; nil}
      body.must_equal 'Hello World'
      header('Content-Length').must_equal '11'
    end
  end

  describe 'redirect' do
    it 'uses a 302 when only a path is given' do
      sin_app do
        redirect '/foo'
        fail 'redirect should halt'
      end

      status.must_equal 302
      body.must_equal ''
      header('Location').must_equal '/foo'
    end

    it 'adds script_name if given a path' do
      sin_app{redirect "/foo"}
      header('Location', '/bar', 'SCRIPT_NAME'=>'/foo').must_equal '/foo'
    end

    it 'does not adds script_name if not given a path' do
      sin_app{redirect}
      header('Location', '/bar', 'SCRIPT_NAME'=>'/foo', 'REQUEST_METHOD'=>'POST').must_equal '/foo/bar'
    end

    it 'respects :absolute_redirects option' do
      sin_app{redirect}
      app.opts[:absolute_redirects] = true
      header('Location', '/bar', 'HTTP_HOST'=>'example.org', 'SCRIPT_NAME'=>'/foo', 'REQUEST_METHOD'=>'POST').must_equal 'http://example.org/foo/bar'
    end

    it 'respects :prefixed_redirects option' do
      sin_app{redirect "/bar"}
      app.opts[:prefixed_redirects] = true
      header('Location', 'SCRIPT_NAME'=>'/foo').must_equal '/foo/bar'
    end

    it 'ignores :prefix_redirects option if not given a path' do
      sin_app{redirect}
      app.opts[:prefix_redirects] = true
      header('Location', "/bar", 'SCRIPT_NAME'=>'/foo', 'REQUEST_METHOD'=>'POST').must_equal '/foo/bar'
    end

    it 'uses the code given when specified' do
      sin_app{redirect '/foo', 301}
      status.must_equal 301
    end

    it 'redirects back to request.referer when passed back' do
      sin_app{redirect back}
      header('Location', 'HTTP_REFERER' => '/foo').must_equal '/foo'
    end

    it 'uses 303 for post requests if request is HTTP 1.1, 302 for 1.0' do
      sin_app{redirect '/foo'}
      status('HTTP_VERSION' => 'HTTP/1.1', 'REQUEST_METHOD'=>'POST').must_equal 303
      status('HTTP_VERSION' => 'HTTP/1.0', 'REQUEST_METHOD'=>'POST').must_equal 302
    end
  end

  describe 'error' do
    it 'sets a status code and halts' do
      sin_app do
        error
        fail 'error should halt'
      end

      status.must_equal 500
      body.must_equal ''
    end

    it 'accepts status code' do
      sin_app{error 501}
      status.must_equal 501
      body.must_equal ''
    end

    it 'accepts body' do
      sin_app{error '501'}
      status.must_equal 500
      body.must_equal '501'
    end

    it 'accepts status code and body' do
      sin_app{error 502, '501'}
      status.must_equal 502
      body.must_equal '501'
    end
  end

  describe 'not_found' do
    it 'halts with a 404 status' do
      sin_app do
        not_found
        fail 'not_found should halt'
      end

      status.must_equal 404
      body.must_equal ''
    end

    it 'accepts optional body' do
      sin_app{not_found 'nf'}
      status.must_equal 404
      body.must_equal 'nf'
    end
  end

  describe 'headers' do
    it 'sets headers on the response object when given a Hash' do
      sin_app do
        headers 'X-Foo' => 'bar'
        'kthx'
      end

      header('X-Foo').must_equal 'bar'
      body.must_equal 'kthx'
    end

    it 'returns the response headers hash when no hash provided' do
      sin_app{headers['X-Foo'] = 'bar'}
      header('X-Foo').must_equal 'bar'
    end
  end

  describe 'mime_type' do
    before do
      sin_app{|r| mime_type(r.path).to_s}
    end

    it "looks up mime types in Rack's MIME registry" do
      Rack::Mime::MIME_TYPES['.foo'] = 'application/foo'
      body('foo').must_equal 'application/foo'
      body('.foo').must_equal 'application/foo'
    end

    it 'returns nil when given nil' do
      body('PATH_INFO'=>nil).must_equal ''
    end

    it 'returns nil when media type not registered' do
      body('bizzle').must_equal ''
    end

    it 'returns the argument when given a media type string' do
      body('text/plain').must_equal 'text/plain'
    end

    it 'supports mime types registered at the class level' do
      app.mime_type :foo, 'application/foo2'
      body('foo').must_equal 'application/foo2'
    end
  end

  describe 'content_type' do
    it 'sets the Content-Type header' do
      sin_app do
        content_type 'text/plain'
        'Hello World'
      end

      header('Content-Type').must_equal 'text/plain'
      body.must_equal 'Hello World'
    end

    it 'takes media type parameters (like charset=)' do
      sin_app{content_type 'text/html', :charset => 'latin1'}
      header('Content-Type').must_equal 'text/html;charset=latin1'
    end

    it "looks up symbols in Rack's mime types dictionary" do
      sin_app{content_type :foo}
      Rack::Mime::MIME_TYPES['.foo'] = 'application/foo'
      header('Content-Type').must_equal 'application/foo'
    end

    it 'fails when no mime type is registered for the argument provided' do
      sin_app{content_type :bizzle}
      proc{body}.must_raise(Roda::RodaError)
    end

    it 'handles already present params' do
      sin_app{content_type 'foo/bar;level=1', :charset => 'utf-8'}
      header('Content-Type').must_equal 'foo/bar;level=1, charset=utf-8'
    end

    it 'does not add charset if present' do
      sin_app{content_type 'text/plain;charset=utf-16', :charset => 'utf-8'}
      header('Content-Type').must_equal 'text/plain;charset=utf-16'
    end

    it 'properly encodes parameters with delimiter characters' do
      sin_app{|r| content_type 'image/png', :comment => r.path }
      header('Content-Type', 'Hello, world!').must_equal 'image/png;comment="Hello, world!"'
      header('Content-Type', 'semi;colon').must_equal 'image/png;comment="semi;colon"'
      header('Content-Type', '"Whatever."').must_equal 'image/png;comment="\"Whatever.\""'
    end
  end

  describe 'attachment' do
    before do
      sin_app{|r| attachment r.path; 'b'}
    end

    it 'sets the Content-Disposition header' do
      header('Content-Disposition', '/foo/test.xml').must_equal 'attachment; filename="test.xml"'
      body.must_equal 'b'
    end

    it 'sets the Content-Disposition header even when a filename is not given' do
      sin_app{attachment}
      header('Content-Disposition', '/foo/test.xml').must_equal 'attachment'
    end

    it 'sets the Content-Type header' do
      header('Content-Type', 'test.xml').must_equal 'application/xml'
    end

    it 'does not modify the default Content-Type without a file extension' do
      header('Content-Type', 'README').must_equal 'text/html'
    end

    it 'should not modify the Content-Type if it is already set' do
      sin_app do
        content_type :atom
        attachment 'test.xml'
      end

      header('Content-Type', 'README').must_equal 'application/atom+xml'
    end
  end

  describe 'send_file' do
    before(:all) do
      file = @file = 'spec/assets/css/raw.css'
      @content = File.read(@file)
      sin_app{send_file file, env['OPTS'] || {}}
    end

    it "sends the contents of the file" do
      status.must_equal 200
      body.must_equal @content
    end

    it 'sets the Content-Type response header if a mime-type can be located' do
      header('Content-Type').must_equal 'text/css'
    end

    it 'sets the Content-Type response header if type option is set to a file extension' do
      header('Content-Type', 'OPTS'=>{:type => 'html'}).must_equal 'text/html'
    end

    it 'sets the Content-Type response header if type option is set to a mime type' do
      header('Content-Type', 'OPTS'=>{:type => 'application/octet-stream'}).must_equal 'application/octet-stream'
    end

    it 'sets the Content-Length response header' do
      header('Content-Length').must_equal @content.length.to_s
    end

    it 'sets the Last-Modified response header' do
      header('Last-Modified').must_equal File.mtime(@file).httpdate
    end

    it 'allows passing in a different Last-Modified response header with :last_modified' do
      time = Time.now
      @app.plugin :caching
      header('Last-Modified', 'OPTS'=>{:last_modified => time}).must_equal time.httpdate
    end

    it "returns a 404 when not found" do
      sin_app{send_file 'this-file-does-not-exist.txt'}
      status.must_equal 404
    end

    it "does not set the Content-Disposition header by default" do
      header('Content-Disposition').must_be_nil
    end

    it "sets the Content-Disposition header when :disposition set to 'attachment'" do
      header('Content-Disposition', 'OPTS'=>{:disposition => 'attachment'}).must_equal 'attachment; filename="raw.css"'
    end

    it "does not set add a file name if filename is false" do
      header('Content-Disposition', 'OPTS'=>{:disposition => 'inline', :filename=>false}).must_equal 'inline'
    end

    it "sets the Content-Disposition header when :disposition set to 'inline'" do
      header('Content-Disposition', 'OPTS'=>{:disposition => 'inline'}).must_equal 'inline; filename="raw.css"'
    end

    it "sets the Content-Disposition header when :filename provided" do
      header('Content-Disposition', 'OPTS'=>{:filename => 'foo.txt'}).must_equal 'attachment; filename="foo.txt"'
    end

    it 'allows setting a custom status code' do
      status('OPTS'=>{:status=>201}).must_equal 201
    end

    it "is able to send files with unknown mime type" do
      header('Content-Type', 'OPTS'=>{:type => '.foobar'}).must_equal 'application/octet-stream'
    end

    it "does not override Content-Type if already set and no explicit type is given" do
      file = @file
      sin_app do
        content_type :png
        send_file file
      end
      header('Content-Type').must_equal 'image/png'
    end

    it "does override Content-Type even if already set, if explicit type is given" do
      file = @file
      sin_app do
        content_type :png
        send_file file, :type => :gif
      end
      header('Content-Type').must_equal 'image/gif'
    end
  end

  describe 'uri' do
    describe "without arguments" do
      before do
        sin_app{uri}
      end

      it 'generates absolute urls' do
        body('HTTP_HOST'=>'example.org').must_equal 'http://example.org/'
      end

      it 'includes path_info' do
        body('/foo', 'HTTP_HOST'=>'example.org').must_equal 'http://example.org/foo'
      end

      it 'includes script_name' do
        body('/bar', 'HTTP_HOST'=>'example.org', "SCRIPT_NAME" => '/foo').must_equal 'http://example.org/foo/bar'
      end

      it 'handles standard HTTP and HTTPS ports' do
        body('SERVER_NAME'=>'example.org', 'SERVER_PORT' => '80').must_equal 'http://example.org/'
        body('SERVER_NAME'=>'example.org', 'SERVER_PORT' => '443', 'HTTPS'=>'on').must_equal 'https://example.org/'
      end

      it 'handles non-standard HTTP port' do
        body('SERVER_NAME'=>'example.org', 'SERVER_PORT' => '81').must_equal 'http://example.org:81/'
        body('SERVER_NAME'=>'example.org', 'SERVER_PORT' => '443').must_equal 'http://example.org:443/'
      end

      it 'handles non-standard HTTPS port' do
        body('SERVER_NAME'=>'example.org', 'SERVER_PORT' => '444', 'HTTPS'=>'on').must_equal 'https://example.org:444/'
        body('SERVER_NAME'=>'example.org', 'SERVER_PORT' => '80', 'HTTPS'=>'on').must_equal 'https://example.org:80/'
      end

      it 'handles reverse proxy' do
        body('SERVER_NAME'=>'example.org', 'HTTP_X_FORWARDED_HOST' => 'example.com', 'SERVER_PORT' => '8080').must_equal 'http://example.com/'
      end
    end

    it 'allows passing an alternative to path_info' do
      sin_app{uri '/bar'}
      body('HTTP_HOST'=>'example.org').must_equal 'http://example.org/bar'
      body('HTTP_HOST'=>'example.org', "SCRIPT_NAME" => '/foo').must_equal 'http://example.org/foo/bar'
    end

    it 'handles absolute URIs' do
      sin_app{uri 'http://google.com'}
      body('HTTP_HOST'=>'example.org').must_equal 'http://google.com'
    end

    it 'handles different protocols' do
      sin_app{uri 'mailto:jsmith@example.com'}
      body('HTTP_HOST'=>'example.org').must_equal 'mailto:jsmith@example.com'
    end

    it 'allows turning off host' do
      sin_app{uri '/foo', false}
      body('HTTP_HOST'=>'example.org').must_equal '/foo'
      body('HTTP_HOST'=>'example.org', "SCRIPT_NAME" => '/bar').must_equal '/bar/foo'
    end

    it 'allows turning off script_name' do
      sin_app{uri '/foo', true, false}
      body('HTTP_HOST'=>'example.org').must_equal 'http://example.org/foo'
      body('HTTP_HOST'=>'example.org', "SCRIPT_NAME" => '/bar').must_equal 'http://example.org/foo'
    end

    it 'is aliased to #url' do
      sin_app{url}
      body('HTTP_HOST'=>'example.org').must_equal 'http://example.org/'
    end

    it 'is aliased to #to' do
      sin_app{to}
      body('HTTP_HOST'=>'example.org').must_equal 'http://example.org/'
    end

    it 'accepts a URI object instead of a String' do
      sin_app{uri URI.parse('http://roda.jeremyevans.net')}
      body.must_equal 'http://roda.jeremyevans.net'
    end
  end

  it 'logger logs to rack.logger' do
    sin_app{logger.info "foo"; nil}
    o = Object.new
    def o.method_missing(*a)
      (@a ||= []) << a
    end
    def o.logs
      @a
    end

    status('rack.logger'=>o).must_equal 404
    o.logs.must_equal [[:info, 'foo']]
  end

  it 'supports disabling delegation if :delegate=>false option is provided' do
    app(:bare) do
      plugin :sinatra_helpers, :delegate=>false
      route do |r|
        r.root{content_type}
        r.is("req"){r.ssl?.to_s}
        r.is("res"){response.not_found?.inspect}
      end
    end

    proc{body}.must_raise(NameError)
    body('/req').must_equal 'false'
    body('/res').must_equal 'nil'
  end
end

