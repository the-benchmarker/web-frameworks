require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "empty_root plugin" do 
  it "makes root match on emtpy path" do
    app(:empty_root) do |r|
      r.root{"root"}
      "notroot"
    end

    body.must_equal 'root'
    body("").must_equal 'root'
    body("a").must_equal 'notroot'
  end
end
