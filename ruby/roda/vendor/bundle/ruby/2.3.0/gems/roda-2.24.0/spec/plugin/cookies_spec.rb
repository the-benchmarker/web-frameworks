require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "cookies plugin" do 
  it "should set cookies on response" do
    app(:cookies) do |r|
      response.set_cookie("foo", "bar")
      response.set_cookie("bar", "baz")
      "Hello"
    end

    header('Set-Cookie').must_equal "foo=bar\nbar=baz"
    body.must_equal 'Hello'
  end

  it "should delete cookies on response" do
    app(:cookies) do |r|
      response.set_cookie("foo", "bar")
      response.delete_cookie("foo")
      "Hello"
    end

    header('Set-Cookie').must_match(/foo=; (max-age=0; )?expires=Thu, 01[ -]Jan[ -]1970 00:00:00 (-0000|GMT)/)
    body.must_equal 'Hello'
  end

  it "should pass default cookie options when setting" do
    app.plugin :cookies, :path => '/foo'
    app.route { response.set_cookie("foo", "bar") }
    header('Set-Cookie').must_equal "foo=bar; path=/foo"

    app.route { response.set_cookie("foo", :value=>"bar", :path=>'/baz') }
    header('Set-Cookie').must_equal "foo=bar; path=/baz"
  end

  it "should pass default cookie options when deleting" do
    app.plugin :cookies, :domain => 'example.com'
    app.route { response.delete_cookie("foo") }
    header('Set-Cookie').must_match(/foo=; domain=example.com; (max-age=0; )?expires=Thu, 01[ -]Jan[ -]1970 00:00:00 (-0000|GMT)/)

    app.route { response.delete_cookie("foo", :domain=>'bar.com') }
    header('Set-Cookie').must_match(/foo=; domain=bar.com; (max-age=0; )?expires=Thu, 01[ -]Jan[ -]1970 00:00:00 (-0000|GMT)/)
  end

  it "should not override existing default cookie options" do
    app.plugin :cookies, :path => '/foo'
    app.plugin :cookies
    app.route { response.set_cookie("foo", "bar") }

    header('Set-Cookie').must_equal "foo=bar; path=/foo"
  end
end
