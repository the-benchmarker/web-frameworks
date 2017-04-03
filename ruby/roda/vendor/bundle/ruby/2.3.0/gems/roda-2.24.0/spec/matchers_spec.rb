require File.expand_path("spec_helper", File.dirname(__FILE__))

describe "capturing" do
  it "doesn't yield the verb" do
    app do |r|
      r.get do |*args|
        args.size.to_s
      end
    end

    body.must_equal '0'
  end

  it "doesn't yield the path" do
    app do |r|
      r.get "home" do |*args|
        args.size.to_s
      end
    end

    body('/home').must_equal '0'
  end

  it "yields the segment" do
    app do |r|
      r.get "user", :id do |id|
        id
      end
    end

    body("/user/johndoe").must_equal 'johndoe'
  end

  it "yields a number" do
    app do |r|
      r.get "user", :id do |id|
        id
      end
    end

    body("/user/101").must_equal '101'
  end

  it "yields a segment per nested block" do
    app do |r|
      r.on :one do |one|
        r.on :two do |two|
          r.on :three do |three|
            one + two + three
          end
        end
      end
    end

    body("/one/two/three").must_equal "onetwothree"
  end

  it "regex captures in regex format" do
    app do |r|
      r.get %r{posts/(\d+)-(.*)} do |id, slug|
        id + slug
      end
    end

    body("/posts/123-postal-service").must_equal "123postal-service"
  end
end

describe "r.is" do 
  it "ensures the patch is matched fully" do
    app do |r|
      r.is "" do
        "+1"
      end
    end

    body.must_equal '+1'
    status('//').must_equal 404
  end

  it "handles no arguments" do
    app do |r|
      r.on "" do
        r.is do
          "+1"
        end
      end
    end

    body.must_equal '+1'
    status('//').must_equal 404
  end

  it "matches strings" do
    app do |r|
      r.is "123" do
        "+1"
      end
    end

    body("/123").must_equal '+1'
    status("/123/").must_equal 404
  end

  it "matches regexps" do
    app do |r|
      r.is(/(\w+)/) do |id|
        id
      end
    end

    body("/123").must_equal '123'
    status("/123/").must_equal 404
  end

  it "matches segments" do
    app do |r|
      r.is :id do |id|
        id
      end
    end

    body("/123").must_equal '123'
    status("/123/").must_equal 404
  end
end

describe "matchers" do
  it "should handle string with embedded param" do
    app do |r|
      r.on "posts/:id" do |id|
        id
      end

      r.on "responses-:id" do |id|
        id
      end
    end
    app.opts[:verbatim_string_matcher] = false

    body('/posts/123').must_equal '123'
    status('/post/123').must_equal 404
    body('/responses-123').must_equal '123'
  end

  it "should not handle string with embedded param if :verbatim_string_matcher option is set" do
    app do |r|
      r.on "posts/:id" do
        '1'
      end

      r.on "responses-:id" do
        '2'
      end
    end
    app.opts[:verbatim_string_matcher] = true

    status('/post/123').must_equal 404
    status('/posts/123').must_equal 404
    body('/posts/:id').must_equal '1'
    status('/responses-123').must_equal 404
    body('/responses-:id').must_equal '2'
  end

  it "should handle multiple params in single string" do
    app do |r|
      r.on "u/:uid/posts/:id" do |uid, id|
        uid + id
      end
    end
    app.opts[:verbatim_string_matcher] = false

    body("/u/jdoe/posts/123").must_equal 'jdoe123'
    status("/u/jdoe/pots/123").must_equal 404
  end

  it "should escape regexp metacharaters in string" do
    app do |r|
      r.on "u/:uid/posts?/:id" do |uid, id|
        uid + id
      end
    end
    app.opts[:verbatim_string_matcher] = false

    body("/u/jdoe/posts?/123").must_equal 'jdoe123'
    status("/u/jdoe/post/123").must_equal 404
  end

  it "should handle colons by themselves" do
    app do |r|
      r.on "u/:/:uid/posts/::id" do |uid, id|
        uid + id
      end
    end
    app.opts[:verbatim_string_matcher] = false

    body("/u/:/jdoe/posts/:123").must_equal 'jdoe123'
    status("/u/a/jdoe/post/b123").must_equal 404
  end

  it "should handle regexes and nesting" do
    app do |r|
      r.on(/u\/(\w+)/) do |uid|
        r.on(/posts\/(\d+)/) do |id|
          uid + id
        end
      end
    end

    body("/u/jdoe/posts/123").must_equal 'jdoe123'
    status("/u/jdoe/pots/123").must_equal 404
  end

  it "should handle regex nesting colon param style" do
    app do |r|
      r.on(/u:(\w+)/) do |uid|
        r.on(/posts:(\d+)/) do |id|
          uid + id
        end
      end
    end

    body("/u:jdoe/posts:123").must_equal 'jdoe123'
    status("/u:jdoe/poss:123").must_equal 404
  end

  it "symbol matching" do
    app do |r|
      r.on "user", :id do |uid|
        r.on "posts", :pid do |id|
          uid + id
        end
      end
    end

    body("/user/jdoe/posts/123").must_equal 'jdoe123'
    status("/user/jdoe/pots/123").must_equal 404
  end

  it "paths and numbers" do
    app do |r|
      r.on "about" do
        r.on :one, :two do |one, two|
          one + two
        end
      end
    end

    body("/about/1/2").must_equal '12'
    status("/about/1").must_equal 404
  end

  it "paths and decimals" do
    app do |r|
     r.on "about" do
        r.on(/(\d+)/) do |one|
          one
        end
      end
    end

    body("/about/1").must_equal '1'
    status("/about/1.2").must_equal 404
  end

  it "should allow arrays to match any value" do
    app do |r|
      r.on [/(\d+)/, /\d+(bar)?/] do |id|
        id
      end
    end

    body('/123').must_equal '123'
    body('/123bar').must_equal 'bar'
    status('/123bard').must_equal 404
  end

  it "should have array capture match string if match" do
    app do |r|
      r.on %w'p q' do |id|
        id
      end
    end

    body('/p').must_equal 'p'
    body('/q').must_equal 'q'
    status('/r').must_equal 404
  end
end

describe "r.on" do 
  it "executes on no arguments" do
    app do |r|
      r.on do
        "+1"
      end
    end

    body.must_equal '+1'
  end

  it "executes on true" do
    app do |r|
      r.on true do
        "+1"
      end
    end

    body.must_equal '+1'
  end

  it "does not execute on false" do
    app do |r|
      r.on false do
        "+1"
      end
    end

    status.must_equal 404
  end

  it "does not execute on nil" do
    app do |r|
      r.on nil do
        "+1"
      end
    end

    status.must_equal 404
  end

  it "executes on arbitrary object" do
    app do |r|
      r.on Object.new do
        "+1"
      end
    end

    body.must_equal '+1'
  end

  it "raises on arbitrary object if :unsupported_matcher => :raise" do
    app(:bare) do 
      opts[:unsupported_matcher] = :raise
      route do |r|
        r.on Object.new do
          "+1"
        end
      end
    end

    proc{body}.must_raise Roda::RodaError
  end

  it "executes on non-false" do
    app do |r|
      r.on "123" do
        "+1"
      end
    end

    body("/123").must_equal '+1'
  end

  it "does not modify SCRIPT_NAME/PATH_INFO during routing" do
    app(:pass) do |r|
      r.on "foo" do
        r.is "bar" do
          "bar|#{env['SCRIPT_NAME']}|#{env['PATH_INFO']}"
        end
        r.is "baz" do
          r.pass
        end
        "foo|#{env['SCRIPT_NAME']}|#{env['PATH_INFO']}"
      end
      "#{env['SCRIPT_NAME']}|#{env['PATH_INFO']}"
    end

    body.must_equal '|/'
    body('SCRIPT_NAME'=>'/a').must_equal '/a|/'
    body('/foo').must_equal 'foo||/foo'
    body('/foo', 'SCRIPT_NAME'=>'/a').must_equal 'foo|/a|/foo'
    body('/foo/bar').must_equal 'bar||/foo/bar'
    body('/foo/bar', 'SCRIPT_NAME'=>'/a').must_equal 'bar|/a|/foo/bar'
    body('/foo/baz').must_equal 'foo||/foo/baz'
    body('/foo/baz', 'SCRIPT_NAME'=>'/a').must_equal 'foo|/a|/foo/baz'
  end

  it "should have path/matched_path/remaining_path work correctly" do
    app do |r|
      r.on "foo" do
        "#{r.path}:#{r.matched_path}:#{r.remaining_path}"
      end
    end

    body("/foo/bar").must_equal  "/foo/bar:/foo:/bar"
  end

  it "ensures remaining_path is reverted if modified in failing matcher" do
    app do |r|
      r.on lambda { @remaining_path = "/blah"; false } do
        "Unreachable"
      end
      
      r.on do
        r.matched_path + ':' + r.remaining_path
      end
    end

    body("/hello").must_equal ':/hello'
  end

  it "modifies matched_path/remaining_path during routing" do
    app do |r|
      r.on 'login', 'foo' do 
        "Unreachable"
      end
      
      r.on 'hello' do
        r.matched_path + ':' + r.remaining_path
      end
    end

    body("/hello/you").must_equal '/hello:/you'
  end

  it "doesn't modify SCRIPT_NAME/PATH_INFO during routing" do
    app do |r|
      r.on 'login', 'foo' do 
        "Unreachable"
      end
      
      r.on 'hello' do
        r.env["SCRIPT_NAME"] + ':' + r.env["PATH_INFO"]
      end
    end

    body("/hello/you").must_equal ':/hello/you'
  end

  it "doesn't mutate SCRIPT_NAME or PATH_INFO after request is returned" do
    app do |r|
      r.on 'login', 'foo' do 
        "Unreachable"
      end
      
      r.on do
        r.env["SCRIPT_NAME"] + ':' + r.env["PATH_INFO"]
      end
    end

    pi, sn = '/login', ''
    env = {"REQUEST_METHOD" => "GET", "PATH_INFO" => pi, "SCRIPT_NAME" => sn}
    app.call(env)[2].join.must_equal ":/login"
    env["PATH_INFO"].must_equal pi 
    env["SCRIPT_NAME"].must_equal sn
  end

  it "skips consecutive matches" do
    app do |r|
      r.on do
        "foo"
      end

      r.on do
        "bar"
      end
    end

    body.must_equal "foo"
  end

  it "finds first match available" do
    app do |r|
      r.on false do
        "foo"
      end

      r.on do
        "bar"
      end
    end

    body.must_equal "bar"
  end

  it "reverts a half-met matcher" do
    app do |r|
      r.on "post", false do
        "Should be unmet"
      end

      r.on do
        r.env["SCRIPT_NAME"] + ':' + r.env["PATH_INFO"]
      end
    end

    body("/hello").must_equal ':/hello'
  end

  it "doesn't write to body if body already written to" do
    app do |r|
      r.on do
        response.write "a"
        "b"
      end
    end

    body.must_equal 'a'
  end
end

describe "path matchers" do 
  it "one level path" do
    app do |r|
      r.on "about" do
        "About"
      end
    end

    body('/about').must_equal "About"
    status("/abot").must_equal 404
  end

  it "two level nested paths" do
    app do |r|
      r.on "about" do
        r.on "1" do
          "+1"
        end

        r.on "2" do
          "+2"
        end
      end
    end

    body('/about/1').must_equal "+1"
    body('/about/2').must_equal "+2"
    status('/about/3').must_equal 404
  end

  it "two level inlined paths" do
    app do |r|
      r.on "a/b" do
        "ab"
      end
    end

    body('/a/b').must_equal "ab"
    status('/a/d').must_equal 404
  end

  it "a path with some regex captures" do
    app do |r|
      r.on(/user(\d+)/) do |uid|
        uid
      end
    end

    body('/user123').must_equal "123"
    status('/useradf').must_equal 404
  end

  it "matching the root with a string" do
    app do |r|
      r.is "" do
        "Home"
      end
    end

    body.must_equal 'Home'
    status("//").must_equal 404
    status("/foo").must_equal 404
  end

  it "matching the root with the root method" do
    app do |r|
      r.root do
        "Home"
      end
    end

    body.must_equal 'Home'
    status('REQUEST_METHOD'=>'POST').must_equal 404
    status("//").must_equal 404
    status("/foo").must_equal 404
  end
end

describe "root/empty segment matching" do
  it "matching an empty segment" do
    app do |r|
      r.on "" do
        r.path
      end
    end

    body.must_equal '/'
    status("/foo").must_equal 404
  end

  it "nested empty segments" do
    app do |r|
      r.on "" do
        r.on "" do
          r.on "1" do
            r.path
          end
        end
      end
    end

    body("///1").must_equal '///1'
    status("/1").must_equal 404
    status("//1").must_equal 404
  end

  it "/events/? scenario" do
    a = app do |r|
      r.on "" do
        "Hooray"
      end

      r.is do
        "Foo"
      end
    end

    app(:new) do |r|
      r.on "events" do
        r.run a
      end
    end

    body("/events").must_equal 'Foo'
    body("/events/").must_equal 'Hooray'
    status("/events/foo").must_equal 404
  end
end

describe "segment handling" do
  before do
    app do |r|
      r.on "post" do
        r.on :id do |id|
          id
        end
      end
    end
  end

  it "matches numeric ids" do
    body('/post/1').must_equal '1'
  end

  it "matches decimal numbers" do
    body('/post/1.1').must_equal '1.1'
  end

  it "matches slugs" do
    body('/post/my-blog-post-about-cuba').must_equal 'my-blog-post-about-cuba'
  end

  it "matches only the first segment available" do
    body('/post/one/two/three').must_equal 'one'
  end
end

describe "request verb methods" do 
  it "executes if verb matches" do
    app do |r|
      r.get do
        "g"
      end
      r.post do
        "p"
      end
    end

    body.must_equal 'g'
    body('REQUEST_METHOD'=>'POST').must_equal 'p'
  end

  it "requires exact match if given arguments" do
    app do |r|
      r.get "" do
        "g"
      end
      r.post "" do
        "p"
      end
    end

    body.must_equal 'g'
    body('REQUEST_METHOD'=>'POST').must_equal 'p'
    status("/a").must_equal 404
    status("/a", 'REQUEST_METHOD'=>'POST').must_equal 404
  end

  it "does not require exact match if given arguments" do
    app do |r|
      r.get do
        r.is "" do
          "g"
        end

        "get"
      end
      r.post do
        r.is "" do
          "p"
        end

        "post"
      end
    end

    body.must_equal 'g'
    body('REQUEST_METHOD'=>'POST').must_equal 'p'
    body("/a").must_equal 'get'
    body("/a", 'REQUEST_METHOD'=>'POST').must_equal 'post'
  end
end

describe "all matcher" do
  it "should match only all all arguments match" do
    app do |r|
      r.is :all=>['foo', :y] do |file|
        file
      end
    end

    body("/foo/bar").must_equal 'bar'
    status.must_equal 404
    status("/foo").must_equal 404
    status("/foo/").must_equal 404
    status("/foo/bar/baz").must_equal 404
  end
end

describe "method matcher" do
  it "should match given request types" do
    app do |r|
      r.is "", :method=>:get do
        "foo"
      end
      r.is "", :method=>[:patch, :post] do
        "bar"
      end
    end

    body("REQUEST_METHOD"=>"GET").must_equal 'foo'
    body("REQUEST_METHOD"=>"PATCH").must_equal 'bar'
    body("REQUEST_METHOD"=>"POST").must_equal 'bar'
    status("REQUEST_METHOD"=>"DELETE").must_equal 404
  end
end

describe "route block that returns string" do
  it "should be treated as if an on block returned string" do
    app do |r|
      "+1"
    end

    body.must_equal '+1'
  end
end

describe "app with :unsupported_block_result => :raise option" do
  def app_value(v)
    app(:bare) do
      opts[:unsupported_block_result] = :raise
      route do |r|
        r.is 'a' do v end
        v
      end
    end
  end

  it "should handle String as body" do
    app_value '1'
    status.must_equal 200
    body.must_equal '1'
    status('/a').must_equal 200
    body('/a').must_equal '1'
  end

  it "should handle nil and false as not found" do
    app_value nil
    status.must_equal 404
    body.must_equal ''
    status('/a').must_equal 404
    body('/a').must_equal ''
  end

  it "should handle false as not found" do
    app_value false
    status.must_equal 404
    body.must_equal ''
    status('/a').must_equal 404
    body('/a').must_equal ''
  end

  it "should raise RodaError for other types" do
    app_value Object.new
    proc{body}.must_raise Roda::RodaError
    proc{body('/a')}.must_raise Roda::RodaError
  end
end
