require "./spec_helper"

it "works" do
  name = ENV["FRAMEWORK"]
  cid = `docker run -td #{name}`.strip
  sleep 20 # due to external program usage
  remote_ip = `docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' #{cid}`.strip
  r = HTTP::Client.get "http://#{remote_ip}:3000/"
  it "should reply with a 200" { r.status_code.should eq 200 }
end
