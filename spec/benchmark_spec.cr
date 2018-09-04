require "./spec_helper"

def get_ip(name)
  cid = `docker run -td #{name}`.strip
  sleep 20 # due to external program usage
  ip = `docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' #{cid}`.strip
  ip
end

it "get on /" do
  name = ENV["FRAMEWORK"]
  remote_ip = get_ip(name)
  r = HTTP::Client.get "http://#{remote_ip}:3000/"
  it "should reply with a 200" { r.status_code.should eq 200 }
  it "should rerturn an empty body" { r.body.should eq "" }
end

it "get on /user/0" do
  name = ENV["FRAMEWORK"]
  remote_ip = get_ip(name)
  r = HTTP::Client.get "http://#{remote_ip}:3000/user/0"
  it "should reply with a 200" { r.status_code.should eq 200 }
  it "should rerturn an empty body" { r.body.should eq "0" }
end

it "post on /user" do
  name = ENV["FRAMEWORK"]
  remote_ip = get_ip(name)
  r = HTTP::Client.post "http://#{remote_ip}:3000/user"
  it "should reply with a 200" { r.status_code.should eq 200 }
  it "should rerturn an empty body" { r.body.should eq "" }
end
