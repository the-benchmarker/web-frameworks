require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "multi_run plugin" do 
  it "adds Roda.run method for setting up prefix delegations to other rack apps" do
    app(:multi_run) do |r|
      r.multi_run
      "c"
    end

    app.run "a", Class.new(Roda).class_eval{route{"a1"}; app}

    body("/a").must_equal 'a1'
    body("/b").must_equal 'c'
    body("/b/a").must_equal 'c'
    body.must_equal 'c'

    app.run "b", Class.new(Roda).class_eval{route{"b1"}; app}

    body("/a").must_equal 'a1'
    body("/b").must_equal 'b1'
    body("/b/a").must_equal 'b1'
    body.must_equal 'c'

    app.run "b/a", Class.new(Roda).class_eval{route{"b2"}; app}

    body("/a").must_equal 'a1'
    body("/b").must_equal 'b1'
    body("/b/a").must_equal 'b2'
    body.must_equal 'c'
  end

  it "works when freezing the app" do
    app(:multi_run) do |r|
      r.multi_run
      "c"
    end

    app.run "a", Class.new(Roda).class_eval{route{"a1"}; app}
    app.run "b", Class.new(Roda).class_eval{route{"b1"}; app}
    app.run "b/a", Class.new(Roda).class_eval{route{"b2"}; app}
    app.freeze

    body("/a").must_equal 'a1'
    body("/b").must_equal 'b1'
    body("/b/a").must_equal 'b2'
    body.must_equal 'c'

    proc{app.run "a", Class.new(Roda).class_eval{route{"a1"}; app}}.must_raise
  end

  it "works when subclassing" do
    app(:multi_run) do |r|
      r.multi_run
      "c"
    end

    app.run "a", Class.new(Roda).class_eval{route{"a1"}; app}
    body("/a").must_equal 'a1'

    a = app
    @app = Class.new(a)

    a.run "b", Class.new(Roda).class_eval{route{"b2"}; app}
    app.run "b", Class.new(Roda).class_eval{route{"b1"}; app}

    body("/a").must_equal 'a1'
    body("/b").must_equal 'b1'

    @app = a
    body("/b").must_equal 'b2'
  end

  it "yields prefix" do
    yielded = false

    app(:multi_run) do |r|
      r.multi_run do |prefix|
        yielded = prefix
      end
    end

    app.run "a", Class.new(Roda).class_eval{route{"a1"}; app}

    body("/a").must_equal "a1"
    yielded.must_equal "a"
  end
end
