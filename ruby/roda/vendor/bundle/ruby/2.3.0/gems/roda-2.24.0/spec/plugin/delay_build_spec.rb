require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "delay_build plugin" do 
  it "does not build rack app until app is called" do
    app(:delay_build){"a"}
    app.instance_variable_get(:@app).must_be_nil
    body.must_equal "a"
    # Work around minitest bug
    refute_equal app.instance_variable_get(:@app), nil
  end

  it "only rebuilds the app if build! is called" do
    app(:delay_build){"a"}
    body.must_equal "a"
    c = Class.new do
      def initialize(_) end
      def call(_) [200, {}, ["b"]] end
    end
    app.use c
    body.must_equal "a"
    app.build!
    body.must_equal "b"
  end
end
