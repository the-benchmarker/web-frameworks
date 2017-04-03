require File.expand_path("spec_helper", File.dirname(__FILE__))

describe "Roda.freeze" do
  before do
    app{}.freeze
  end

  it "should make opts not be modifiable after calling finalize!" do
    proc{app.opts[:foo] = 'bar'}.must_raise
  end

  it "should make use and route raise errors" do
    proc{app.use Class.new}.must_raise
    proc{app.route{}}.must_raise
  end

  it "should make plugin raise errors" do
    proc{app.plugin Module.new}.must_raise
  end

  it "should make subclassing raise errors" do
    proc{Class.new(app)}.must_raise
  end

  it "should freeze app" do
    app.frozen?.must_equal true
  end
end
