require File.expand_path("spec_helper", File.dirname(__FILE__))

describe "r.run" do
  it "should allow composition of apps" do
    a = app do |r|
      r.on "services", :id do |id|
        "View #{id}"
      end
    end

    app(:new) do |r|
      r.on "provider" do
        r.run a
      end
    end

    body("/provider/services/101").must_equal 'View 101'
  end

  it "modifies SCRIPT_NAME/PATH_INFO when calling run" do
    a = app{|r| "#{r.script_name}|#{r.path_info}"}
    app(:static_path_info){|r| r.on("a"){r.run a}}
    body("/a/b").must_equal "/a|/b"
  end

  it "restores SCRIPT_NAME/PATH_INFO before returning from run" do
    a = app{|r| "#{r.script_name}|#{r.path_info}"}
    app(:static_path_info){|r| s = catch(:halt){r.on("a"){r.run a}}; "#{s[2].join}%#{r.script_name}|#{r.path_info}"}
    body("/a/b").must_equal "/a|/b%|/a/b"
  end
end
