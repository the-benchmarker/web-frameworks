require "./spec_helper"

describe Application do
  with_server do
    it "should welcome you" do
      result = curl("GET", "/")
      result.body.should eq("")
    end

    it "should list users" do
      result = curl("GET", "/user")
      result.body.should eq("")
    end

    it "should list users" do
      result = curl("GET", "/user/test123")
      result.body.should eq("test123")
    end
  end
end
