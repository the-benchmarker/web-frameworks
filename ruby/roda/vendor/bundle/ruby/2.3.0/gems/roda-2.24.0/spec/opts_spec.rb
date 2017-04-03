require File.expand_path("spec_helper", File.dirname(__FILE__))

describe "opts" do
  it "is inheritable and allows overriding" do
    c = Class.new(Roda)
    c.opts[:foo] = "bar"
    c.opts[:foo].must_equal "bar"

    sc = Class.new(c)
    sc.opts[:foo].must_equal "bar"

    sc.opts[:foo] = "baz"
    sc.opts[:foo].must_equal "baz"
    c.opts[:foo].must_equal "bar"
  end

  it "should be available as an instance methods" do
    app(:bare) do
      opts[:hello] = "Hello World"

      route do |r|
        r.on do
          opts[:hello]
        end
      end
    end

    body.must_equal "Hello World"
  end

  it "should only shallow clone by default" do
    c = Class.new(Roda)
    c.opts[:foo] = "bar".dup
    c.opts[:foo].must_equal "bar"

    sc = Class.new(c)
    sc.opts[:foo].replace("baz")

    sc.opts[:foo].must_equal "baz"
    c.opts[:foo].must_equal "baz"
  end
end
