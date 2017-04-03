require File.expand_path("spec_helper", File.dirname(__FILE__))

describe "Roda#env" do
  it "should return the environment" do
    app do |r|
      env['PATH_INFO']
    end

    body("/foo").must_equal  "/foo"
  end
end
