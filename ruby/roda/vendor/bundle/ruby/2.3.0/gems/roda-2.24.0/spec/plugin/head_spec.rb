require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "head plugin" do 
  it "considers HEAD requests as GET requests which return no body" do
    app(:head) do |r|
      r.root do
        'root'
      end

      r.get 'a' do
        'a'
      end

      r.is 'b', :method=>[:get, :post] do
        'b'
      end
    end

    s, h, b = req
    s.must_equal 200
    h['Content-Length'].must_equal '4'
    b.must_equal ['root']

    s, h, b = req('REQUEST_METHOD' => 'HEAD')
    s.must_equal 200
    h['Content-Length'].must_equal '4'
    b.must_equal []

    body('/a').must_equal 'a'
    status('/a', 'REQUEST_METHOD' => 'HEAD').must_equal 200

    body('/b').must_equal 'b'
    status('/b', 'REQUEST_METHOD' => 'HEAD').must_equal 200
  end
end
