require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "per_thread_caching plugin" do 
  it "should use a per thread cache instead of a shared cache" do
    app(:bare) do
      plugin :per_thread_caching
      @c = thread_safe_cache
      def self.c; @c end
      route do |r|
        r.on :id do |i|
          ((self.class.c[i] ||= []) << 2).join
        end
      end
    end

    (0..10).map do |n|
      Thread.new do
        Thread.current[:n] = n
        _(body('/a')).must_equal '2'
        _(body('/a')).must_equal '22'
        _(body('/a')).must_equal '222'
        _(body('/b')).must_equal '2'
        _(body('/b')).must_equal '22'
        _(body('/b')).must_equal '222'
      end
    end.map{|t| t.join}
  end
end
