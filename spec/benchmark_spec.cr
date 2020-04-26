require "json"
require "./spec_helper"

def get_ip(name)
  if name == "swift.vapor"
    name = "swift.vapor-framework"
  elsif name == "swift.swifter"
    name = "swift.swifter-framework"
  end
  infos = name.split(".")
  language = infos.shift
  framework = infos.join(".")
  path = File.join(language, framework, "ip.txt")
  File.read(path).strip
end

describe "get on /" do
  name = ENV["FRAMEWORK"]
  remote_ip = get_ip(name)
  r = HTTP::Client.get "http://#{remote_ip}:3000/"
  it "should return successfully" { r.status.success?.should be_true }
  it "should return an empty body" { r.body.should eq "" }
end

describe "get on /user/0" do
  name = ENV["FRAMEWORK"]
  remote_ip = get_ip(name)
  r = HTTP::Client.get "http://#{remote_ip}:3000/user/0"
  it "should return successfully" { r.status.success?.should be_true }
  it "should return <0>" { r.body.should eq "0" }
end

describe "post on /user" do
  name = ENV["FRAMEWORK"]
  remote_ip = get_ip(name)
  r = HTTP::Client.post "http://#{remote_ip}:3000/user"
  it "should return successfully" { r.status.success?.should be_true }
  it "should return an empty body" { r.body.should eq "" }
end
