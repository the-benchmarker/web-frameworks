require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "streaming plugin" do 
  it "adds stream method for streaming responses" do
    app(:streaming) do |r|
      stream do |out|
        %w'a b c'.each{|v| out << v; out.write(v) }
      end
    end

    s, h, b = req
    s.must_equal 200
    h.must_equal('Content-Type'=>'text/html')
    b.to_a.must_equal %w'a a b b c c'
  end

  it "should handle errors when streaming, and run callbacks" do
    a = []
    app(:streaming) do |r|
      stream(:callback=>proc{a << 'e'}) do |out|
        %w'a b'.each{|v| out << v}
        raise Roda::RodaError, 'foo'
        out << 'c'
      end
    end

    s, h, b = req
    s.must_equal 200
    h.must_equal('Content-Type'=>'text/html')
    b.callback{a << 'd'}
    proc{b.each{|v| a << v}}.must_raise(Roda::RodaError)
    a.must_equal %w'a b e d'
  end

  it "should handle :loop option to loop" do
    a = []
    app(:streaming) do |r|
      b = %w'a b c'
      stream(:loop=>true, :callback=>proc{a << 'e'}) do |out|
        out << b.shift
        raise Roda::RodaError, 'foo' if b.length == 1
      end
    end

    s, h, b = req
    s.must_equal 200
    h.must_equal('Content-Type'=>'text/html')
    b.callback{a << 'd'}
    proc{b.each{|v| a << v}}.must_raise(Roda::RodaError)
    a.must_equal %w'a b e d'
  end

  it "uses handle_stream_error for handling errors when streaming" do
    a = []
    app(:streaming) do |r|
      b = %w'a b c'
      stream(:loop=>true, :callback=>proc{a << 'e'}) do |out|
        out << b.shift
        raise Roda::RodaError, 'foo' if b.length == 1
      end
    end

    app.send(:define_method, :handle_stream_error) do |error, out|
      out << '1'
      raise error
    end

    s, h, b = req
    s.must_equal 200
    h.must_equal('Content-Type'=>'text/html')
    b.callback{a << 'd'}
    proc{b.each{|v| a << v}}.must_raise(Roda::RodaError)
    a.must_equal %w'a b 1 e d'
  end

  it "should allow closing the stream when handling an error" do
    a = []
    app(:streaming) do |r|
      b = %w'a b c'
      stream(:loop=>true, :callback=>proc{a << 'e'}) do |out|
        out << b.shift
        raise Roda::RodaError, 'foo' if b.length == 1
      end
    end

    app.send(:define_method, :handle_stream_error) do |error, out|
      out.close
    end

    s, h, b = req
    s.must_equal 200
    h.must_equal('Content-Type'=>'text/html')
    b.callback{a << 'd'}
    b.each{|v| a << v}
    a.must_equal %w'a b e d'
  end

  it "should allow ignoring errors when streaming" do
    a = []
    b2 = %w'a b c'

    app(:streaming) do |r|
      stream(:loop=>true, :callback=>proc{a << 'e'}) do |out|
        out << b2.shift
        raise Roda::RodaError
      end
    end

    app.send(:define_method, :handle_stream_error) do |error, out|
      out << '1'
      out.close if b2.empty?
    end

    s, h, b = req
    s.must_equal 200
    h.must_equal('Content-Type'=>'text/html')
    b.callback{a << 'd'}
    b.each{|v| a << v}
    a.must_equal %w'a 1 b 1 c 1 e d'
  end
end
