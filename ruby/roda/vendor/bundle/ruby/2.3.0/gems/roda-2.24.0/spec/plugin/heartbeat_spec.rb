require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "heartbeat plugin" do 
  it "should return heartbeat response for heartbeat paths only" do
    app(:bare) do
      plugin :heartbeat
      route do |r|
        r.on 'a' do
          "a"
        end
      end
    end

    body('/a').must_equal 'a'
    status.must_equal 404
    status('/heartbeat').must_equal 200
    body('/heartbeat').must_equal 'OK'
  end

  it "should support custom heartbeat paths" do
    app(:bare) do
      plugin :heartbeat, :path=>'/heartbeat2'
      route do |r|
        r.on 'a' do
          "a"
        end
      end
    end

    body('/a').must_equal 'a'
    status.must_equal 404
    status('/heartbeat').must_equal 404
    status('/heartbeat2').must_equal 200
    body('/heartbeat2').must_equal 'OK'
  end

  it "should work when using sessions" do
    app(:bare) do
      use Rack::Session::Cookie, :secret=>'foo'
      plugin :heartbeat

      route do |r|
        session.clear
        r.on "a" do
          "a"
        end
      end
    end

    body('/a').must_equal 'a'
    status.must_equal 404
    status('/heartbeat').must_equal 200
    body('/heartbeat').must_equal 'OK'
  end

  it "should work when redirecting" do
    app(:bare) do
      plugin :heartbeat

      route do |r|
        r.on "a" do
          "a"
        end
        r.redirect '/a'
      end
    end

    body('/a').must_equal 'a'
    status.must_equal 302
    status('/heartbeat').must_equal 200
    body('/heartbeat').must_equal 'OK'
  end
end

