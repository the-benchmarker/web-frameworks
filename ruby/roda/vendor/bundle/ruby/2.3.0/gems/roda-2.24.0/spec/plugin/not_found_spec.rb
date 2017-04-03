require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "not_found plugin" do 
  it "executes on no arguments" do
    app(:bare) do
      plugin :not_found

      not_found do
        "not found"
      end

      route do |r|
        r.on "a" do
          "found"
        end
      end
    end

    body.must_equal 'not found'
    status.must_equal 404
    body("/a").must_equal 'found'
    status("/a").must_equal 200
  end

  it "allows overriding status inside not_found" do
    app(:bare) do
      plugin :not_found

      not_found do
        response.status = 403
        "not found"
      end

      route do |r|
      end
    end

    status.must_equal 403
  end

  it "calculates correct Content-Length" do
    app(:bare) do
      plugin :not_found do
        "a"
      end

      route{}
    end

    header('Content-Length').must_equal "1"
  end

  it "clears existing headers" do
    app(:bare) do
      plugin :not_found do ||
        "a"
      end

      route do |r|
        response['Content-Type'] = 'text/pdf'
        response['Foo'] = 'bar'
        nil
      end
    end

    header('Content-Type').must_equal 'text/html'
    header('Foo').must_be_nil
  end

  it "does not modify behavior if not_found is not called" do
    app(:not_found) do |r|
      r.on "a" do
        "found"
      end
    end

    body.must_equal ''
    body("/a").must_equal 'found'
  end

  it "can set not_found via the plugin block" do
    app(:bare) do
      plugin :not_found do
        "not found"
      end

      route do |r|
        r.on "a" do
          "found"
        end
      end
    end

    body.must_equal 'not found'
    body("/a").must_equal 'found'
  end

  it "does not modify behavior if body is not an array" do
    app(:bare) do
      plugin :not_found do
        "not found"
      end

      o = Object.new
      def o.each; end
      route do |r|
        r.halt [404, {}, o]
      end
    end

    body.must_equal ''
  end

  it "does not modify behavior if body is not an empty array" do
    app(:bare) do
      plugin :not_found do
        "not found"
      end

      route do |r|
        response.status = 404
        response.write 'a'
      end
    end

    body.must_equal 'a'
  end
end
