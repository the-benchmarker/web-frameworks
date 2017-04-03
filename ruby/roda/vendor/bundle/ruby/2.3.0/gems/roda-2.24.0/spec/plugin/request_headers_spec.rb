require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "request_headers plugin" do
  def header_app(header_name)
    app(:bare) do
      plugin :request_headers
      route do |r|
        r.on do
          # return the value of the request header in the response body,
          # or the static string 'not found' if it hasn't been supplied.
          r.headers[header_name] || 'not found'
        end
      end
    end
  end

  it "must add HTTP_ prefix when appropriate" do
    header_app('Foo')
    body('/', {'HTTP_FOO' => 'a'}).must_equal 'a'
  end

  it "must ignore HTTP_ prefix when appropriate" do
    header_app('Content-Type')
    body('/', {'CONTENT_TYPE' => 'a'}).must_equal 'a'
  end

  it "must return nil for non-existant headers" do
    header_app('X-Non-Existant')
    body('/').must_equal 'not found'
  end

  it "must be case-insensitive" do
    header_app('X-My-Header')
    body('/', {'HTTP_X_MY_HEADER' => 'a'}).must_equal 'a'

    header_app('x-my-header')
    body('/', {'HTTP_X_MY_HEADER' => 'a'}).must_equal 'a'
  end
end
