require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "error_handler plugin" do 
  it "executes only if error raised" do
    app(:bare) do
      plugin :error_handler

      error do |e|
        e.message
      end

      route do |r|
        r.on "a" do
          "found"
        end

        raise ArgumentError, "bad idea"
      end
    end

    body("/a").must_equal 'found'
    status("/a").must_equal 200
    body.must_equal 'bad idea'
    status.must_equal 500
  end

  it "executes on SyntaxError exceptions" do
    app(:bare) do
      plugin :error_handler

      error do |e|
        e.message
      end

      route do |r|
        r.on "a" do
          "found"
        end

        raise SyntaxError, 'bad idea'
      end
    end

    body("/a").must_equal 'found'
    status("/a").must_equal 200
    body.must_equal 'bad idea'
    status.must_equal 500
  end

  it "executes on custom exception classes" do
    app(:bare) do
      plugin :error_handler, :classes=>[StandardError]

      error do |e|
        e.message
      end

      route do |r|
        r.on "a" do
          raise 'foo'
        end

        raise SyntaxError, 'bad idea'
      end
    end

    proc{body}.must_raise SyntaxError
    body("/a").must_equal 'foo'

    @app = Class.new(@app)
    proc{body}.must_raise SyntaxError
    body("/a").must_equal 'foo'
  end

  it "can override status inside error block" do
    app(:bare) do
      plugin :error_handler do |e|
        response.status = 501
        e.message
      end

      route do |r|
        raise ArgumentError, "bad idea"
      end
    end

    status.must_equal 501
  end

  it "calculates correct Content-Length" do
    app(:bare) do
      plugin :error_handler do |e|
        "a"
      end

      route do |r|
        raise ArgumentError, "bad idea"
      end
    end

    header('Content-Length').must_equal "1"
  end

  it "clears existing headers" do
    app(:bare) do
      plugin :error_handler do |e|
        "a"
      end

      route do |r|
        response['Content-Type'] = 'text/pdf'
        response['Foo'] = 'bar'
        raise ArgumentError, "bad idea"
      end
    end

    header('Content-Type').must_equal 'text/html'
    header('Foo').must_be_nil
  end

  it "can set error via the plugin block" do
    app(:bare) do
      plugin :error_handler do |e|
        e.message
      end

      route do |r|
        raise ArgumentError, "bad idea"
      end
    end

    body.must_equal 'bad idea'
  end

  it "has default error handler also raise" do
    app(:bare) do
      plugin :error_handler

      route do |r|
        raise ArgumentError, "bad idea"
      end
    end

    proc{req}.must_raise(ArgumentError)
  end

  it "has access to current remaining_path" do
    app(:bare) do
      plugin :error_handler do |e|
        request.remaining_path
      end

      route do |r|
        r.on('a') do
          raise ArgumentError, "bad idea"
        end

        raise ArgumentError, "bad idea"
      end
    end

    body.must_equal '/'
    body('/b').must_equal '/b'
    body('/a').must_equal ''
    body('/a/c').must_equal '/c'
  end
end
