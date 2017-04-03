require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "param_matchers plugin" do 
  it "param! matcher should yield a param only if given and not empty" do
    app(:param_matchers) do |r|
      r.get "signup", :param! => "email" do |email|
        email
      end

      "No email"
    end

    io = StringIO.new
    body("/signup", "rack.input" => io, "QUERY_STRING" => "email=john@doe.com").must_equal 'john@doe.com'
    body("/signup", "rack.input" => io, "QUERY_STRING" => "").must_equal 'No email'
    body("/signup", "rack.input" => io, "QUERY_STRING" => "email=").must_equal 'No email'
  end

  it "param matcher should yield a param only if given" do
    app(:param_matchers) do |r|
      r.get "signup", :param=>"email" do |email|
        email
      end

      "No email"
    end

    io = StringIO.new
    body("/signup", "rack.input" => io, "QUERY_STRING" => "email=john@doe.com").must_equal 'john@doe.com'
    body("/signup", "rack.input" => io, "QUERY_STRING" => "").must_equal 'No email'
    body("/signup", "rack.input" => io, "QUERY_STRING" => "email=").must_equal ''
  end

  it "params! matcher should yield the params only if all are given and not empty" do
    app(:param_matchers) do |r|
      r.get "signup", :params! => %w"em ail" do |em, ail|
        em + ail
      end

      "No email"
    end

    io = StringIO.new
    body("/signup", "rack.input" => io, "QUERY_STRING" => "em=foo&ail=john@doe.com").must_equal 'foojohn@doe.com'
    body("/signup", "rack.input" => io, "QUERY_STRING" => "em=&ail=john@doe.com").must_equal 'No email'
    body("/signup", "rack.input" => io, "QUERY_STRING" => "em=foo&ail=").must_equal 'No email'
    body("/signup", "rack.input" => io, "QUERY_STRING" => "em=&ail=").must_equal 'No email'
    body("/signup", "rack.input" => io, "QUERY_STRING" => "em=foo").must_equal 'No email'
    body("/signup", "rack.input" => io, "QUERY_STRING" => "ail=john@doe.com").must_equal 'No email'
  end

  it "params matcher should yield the params only if all are given" do
    app(:param_matchers) do |r|
      r.get "signup", :params=>%w"em ail" do |em, ail|
        em + ail
      end

      "No email"
    end

    io = StringIO.new
    body("/signup", "rack.input" => io, "QUERY_STRING" => "em=foo&ail=john@doe.com").must_equal 'foojohn@doe.com'
    body("/signup", "rack.input" => io, "QUERY_STRING" => "em=&ail=john@doe.com").must_equal 'john@doe.com'
    body("/signup", "rack.input" => io, "QUERY_STRING" => "em=foo&ail=").must_equal 'foo'
    body("/signup", "rack.input" => io, "QUERY_STRING" => "em=&ail=").must_equal ''
    body("/signup", "rack.input" => io, "QUERY_STRING" => "em=foo").must_equal 'No email'
    body("/signup", "rack.input" => io, "QUERY_STRING" => "ail=john@doe.com").must_equal 'No email'
  end
end
