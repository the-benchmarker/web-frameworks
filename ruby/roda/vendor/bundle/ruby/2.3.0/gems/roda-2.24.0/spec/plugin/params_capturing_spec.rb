require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "params_capturing plugin" do 
  it "should add captures to r.params for symbol matchers" do
    app(:params_capturing) do |r|
      r.on('foo', :y, :z, :w) do |y, z, w|
        (r.params.values_at('y', 'z', 'w') + [y, z, w, r[:captures].length]).join('-')
      end

      r.on(/(quux)/, /(foo)(bar)/) do |q, foo, bar|
        "y-#{r[:captures].join}-#{q}-#{foo}-#{bar}"
      end

      r.on(/(quux)/, :y) do |q, y|
        r.on(:x) do |x|
          "y-#{r[:y]}-#{r[:x]}-#{q}-#{y}-#{x}-#{r[:captures].length}"
        end

        "y-#{r[:y]}-#{q}-#{y}-#{r[:captures].length}"
      end

      r.on(:x) do |x|
        "x-#{x}-#{r[:x]}-#{r[:captures].length}"
      end
    end

    body('/blarg', 'rack.input'=>StringIO.new).must_equal 'x-blarg-blarg-1'
    body('/foo/1/2/3', 'rack.input'=>StringIO.new).must_equal '1-2-3-1-2-3-3'
    body('/quux/foobar', 'rack.input'=>StringIO.new).must_equal 'y-quuxfoobar-quux-foo-bar'
    body('/quux/asdf', 'rack.input'=>StringIO.new).must_equal 'y--quux-asdf-2'
    body('/quux/asdf/890', 'rack.input'=>StringIO.new).must_equal 'y--890-quux-asdf-890-3'
  end

  it "should add captures to r.params for string matchers" do
    app(:params_capturing) do |r|
      r.on("bar/:foo") do |foo|
        "b-#{foo}-#{r[:foo]}-#{r[:captures].length}"
      end

      r.on("baz/:bar", :foo) do |bar, foo|
        "b-#{bar}-#{foo}-#{r[:bar]}-#{r[:foo]}-#{r[:captures].length}"
      end
    end
    app.opts[:verbatim_string_matcher] = false

    body('/bar/banana', 'rack.input'=>StringIO.new).must_equal 'b-banana-banana-1'
    body('/baz/ban/ana', 'rack.input'=>StringIO.new).must_equal 'b-ban-ana-ban-ana-2'
  end
end
