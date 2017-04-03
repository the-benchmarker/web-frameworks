require File.expand_path("spec_helper", File.dirname(__FILE__))

describe "response #[] and #[]=" do
  it "should get/set headers" do
    app do |r|
      response['foo'] = 'bar'
      response['foo'] + response.headers['foo']
    end

    header('foo').must_equal "bar"
    body.must_equal 'barbar'
  end
end

describe "response #headers and #body" do
  it "should return headers and body" do
    app do |r|
      response.headers['foo'] = 'bar'
      response.write response.body.is_a?(Array)
    end

    header('foo').must_equal "bar"
    body.must_equal 'true'
  end
end

describe "response #write" do
  it "should add to body" do
    app do |r|
      response.write 'a'
      response.write 'b'
    end

    body.must_equal 'ab'
  end
end

describe "response #finish" do
  it "should set status to 404 if body has not been written to" do
    app do |r|
      s, h, b = response.finish
      "#{s}#{h['Content-Type']}#{b.length}"
    end

    body.must_equal '404text/html0'
  end

  it "should set status to 200 if body has been written to" do
    app do |r|
      response.write 'a'
      s, h, b = response.finish
      response.write "#{s}#{h['Content-Type']}#{b.length}"
    end

    body.must_equal 'a200text/html1'
  end

  it "should set Content-Length header" do
    app do |r|
      response.write 'a'
      response['Content-Length'].must_be_nil
      throw :halt, response.finish
    end

    header('Content-Length').must_equal '1'
  end

  it "should not set Content-Type header on a 204 response" do
    app do |r|
      response.status = 204
      throw :halt, response.finish
    end

    header('Content-Type').must_be_nil
    header('Content-Length').must_be_nil
  end

  it "should not overwrite existing status" do
    app do |r|
      response.status = 500
      s, h, b = response.finish
      "#{s}#{h['Content-Type']}#{b.length}"
    end

    body.must_equal '500text/html0'
  end
end

describe "response #finish_with_body" do
  it "should use given body" do
    app do |r|
      throw :halt, response.finish_with_body(['123'])
    end

    body.must_equal '123'
  end

  it "should set status to 200 if status has not been set" do
    app do |r|
      throw :halt, response.finish_with_body([])
    end

    status.must_equal 200
  end

  it "should not set Content-Length header" do
    app do |r|
      response.write 'a'
      response['Content-Length'].must_be_nil
      throw :halt, response.finish_with_body(['123'])
    end

    header('Content-Length').must_be_nil
  end

  it "should not overwrite existing status" do
    app do |r|
      response.status = 500
      throw :halt, response.finish_with_body(['123'])
    end

    status.must_equal 500
  end
end

describe "response #redirect" do
  it "should set location and status" do
    app do |r|
      r.on 'a' do
        response.redirect '/foo', 303
      end
      r.on do
        response.redirect '/bar'
      end
    end

    status('/a').must_equal 303
    status.must_equal 302
    header('Location', '/a').must_equal '/foo'
    header('Location').must_equal '/bar'
  end
end

describe "response #empty?" do
  it "should return whether the body is empty" do
    app do |r|
      r.on 'a' do
        response['foo'] = response.empty?.to_s
      end
      r.on do
        response.write 'a'
        response['foo'] = response.empty?.to_s
      end
    end

    header('foo', '/a').must_equal 'true'
    header('foo').must_equal 'false'
  end
end

describe "response #inspect" do
  it "should return information about response" do
    app(:bare) do
      def self.inspect
        'Foo'
      end

      route do |r|
        response.status = 200
        response.inspect
      end
    end

    body.must_equal '#<Foo::RodaResponse 200 {} []>'
  end
end

describe "roda_class" do
  it "should return the related roda subclass" do
    app do |r|
      self.class.opts[:a] = 'a'
      response.class.roda_class.opts[:a] + response.roda_class.opts[:a]
    end

    body.must_equal  "aa"
  end
end
