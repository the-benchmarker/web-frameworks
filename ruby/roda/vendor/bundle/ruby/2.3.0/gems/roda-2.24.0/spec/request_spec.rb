require File.expand_path("spec_helper", File.dirname(__FILE__))

describe "request.path, .remaining_path, and .matched_path" do
  it "should return the script name and path_info as a string" do
    app do |r|
      r.on "foo" do
        "#{r.path}:#{r.matched_path}:#{r.remaining_path}"
      end
    end

    body("/foo/bar").must_equal  "/foo/bar:/foo:/bar"
  end
end

describe "request.real_remaining_path" do
  it "should be an alias of remaining_path" do
    app do |r|
      r.on "foo" do
        "#{r.remaining_path}:#{r.real_remaining_path}"
      end
    end

    body("/foo/bar").must_equal "/bar:/bar"
  end
end

describe "request.halt" do
  it "should return rack response as argument given it as argument" do
    app do |r|
      r.halt [200, {}, ['foo']]
    end

    body.must_equal  "foo"
  end

  it "should use current response if no argument is given" do
    app do |r|
      response.write('foo')
      r.halt
    end

    body.must_equal  "foo"
  end
end

describe "request.scope" do
  it "should return roda instance" do
    app(:bare) do
      attr_accessor :b

      route do |r|
        self.b = 'a'
        request.scope.b
      end
    end

    body.must_equal  "a"
  end
end

describe "request.inspect" do
  it "should return information about request" do
    app(:bare) do
      def self.inspect
        'Foo'
      end

      route do |r|
        request.inspect
      end
    end

    body('/a/b').must_equal  "#<Foo::RodaRequest GET /a/b>"
    body('REQUEST_METHOD'=>'POST').must_equal  "#<Foo::RodaRequest POST />"
  end
end

describe "TERM.inspect" do
  it "should return TERM" do
    app do |r|
      r.class::TERM.inspect
    end

    body.must_equal  "TERM"
  end
end

describe "roda_class" do
  it "should return the related roda subclass" do
    app do |r|
      self.class.opts[:a] = 'a'
      r.class.roda_class.opts[:a] + r.roda_class.opts[:a]
    end

    body.must_equal  "aa"
  end
end
