require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "environments plugin" do 
  before do
    app
    app.plugin :environments, :development
  end

  it "adds environment accessor for getting/setting the environment" do
    app.environment.must_equal :development
    app.environment = :test
    app.environment.must_equal :test
    
    app.plugin :environments, :production
    app.environment.must_equal :production
  end

  it "adds predicates for testing the environment" do
    app.development?.must_equal true
    app.test?.must_equal false
    app.production?.must_equal false
  end

  it "adds configure method which yields if no arguments are given or an environment matches" do
    a = []
    app.configure{a << 1}
    app.configure(:development){|ap| a << ap}
    app.configure(:test, :production){a << 2}
    a.must_equal [1, app]
  end

  it "defaults environment to RACK_ENV" do
    with_rack_env('test') do
      app(:environments){}
    end
    app.test?.must_equal true
    app.development?.must_equal false
    app(:environments){}
    app.test?.must_equal false
    app.development?.must_equal true
  end
end
