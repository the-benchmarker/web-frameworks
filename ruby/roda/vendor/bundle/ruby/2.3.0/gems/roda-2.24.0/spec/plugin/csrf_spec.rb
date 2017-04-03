require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

begin
  require 'rack/csrf'
rescue LoadError
  warn "rack_csrf not installed, skipping csrf plugin test"  
else
describe "csrf plugin" do 
  it "adds csrf protection and csrf helper methods" do
    app(:bare) do
      use Rack::Session::Cookie, :secret=>'1'
      plugin :csrf, :skip=>['POST:/foo']

      route do |r|
        r.get do
          response['TAG'] = csrf_tag
          response['METATAG'] = csrf_metatag
          response['TOKEN'] = csrf_token
          response['FIELD'] = csrf_field
          response['HEADER'] = csrf_header
          'g'
        end
        r.post 'foo' do
          'bar'
        end
        r.post do
          'p'
        end
      end
    end

    io = StringIO.new
    status('REQUEST_METHOD'=>'POST', 'rack.input'=>io).must_equal 403
    body('/foo', 'REQUEST_METHOD'=>'POST', 'rack.input'=>io).must_equal 'bar'

    env = proc{|h| h['Set-Cookie'] ? {'HTTP_COOKIE'=>h['Set-Cookie'].sub("; path=/; HttpOnly", '')} : {}}
    s, h, b = req
    s.must_equal 200
    field = h['FIELD']
    token = Regexp.escape(h['TOKEN'])
    h['TAG'].must_match(/\A<input type="hidden" name="#{field}" value="#{token}" \/>\z/)
    h['METATAG'].must_match(/\A<meta name="#{field}" content="#{token}" \/>\z/)
    b.must_equal ['g']
    s, _, b = req('/', env[h].merge('REQUEST_METHOD'=>'POST', 'rack.input'=>io, "HTTP_#{h['HEADER']}"=>h['TOKEN']))
    s.must_equal 200
    b.must_equal ['p']

    app.plugin :csrf
    body('/foo', 'REQUEST_METHOD'=>'POST', 'rack.input'=>io).must_equal 'bar'
  end
end
end
