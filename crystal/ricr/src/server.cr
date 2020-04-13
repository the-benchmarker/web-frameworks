require "ricr"

struct ID < Ricr::Parameters::Path
  getter value : String

  def initialize(@value)
  end

  def self.from_string(str)
    new str
  end
end

controller = Ricr::Controller::Default.new.add do
  get { }

  post "user", request_body: String do
  end

  get "user", ID do |params|
    params[ID].value
  end
end

server = Ricr.new "0.0.0.0", 3000, controller: controller, reuse_port: true 

System.cpu_count.times do |i|
  Process.fork do
    server.start
  end
end

sleep
