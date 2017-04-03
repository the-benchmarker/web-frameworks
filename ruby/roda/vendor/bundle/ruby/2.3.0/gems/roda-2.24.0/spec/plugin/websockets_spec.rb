require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

if RUBY_VERSION >= '1.9.3'
begin
  lib = nil
  for lib in %w'faye/websocket thin' do
    require lib
  end
rescue LoadError
  warn "#{lib} not installed, skipping websockets plugin test"  
else
describe "websockets plugin" do 
  it "supports regular requests" do
    app(:websockets) do |r|
      r.websocket{}
      "a"
    end
    body.must_equal 'a'
  end
end

describe "websockets plugin" do 
  before do
    events = @events = []
    app(:bare) do
      plugin :websockets, :adapter=>:thin
      route do |r|
        r.websocket do |ws|
          ws.on(:open) do |event|
            events << 'open'
          end
          ws.on(:message) do |event|
            events << event.data
            ws.send(event.data.reverse)
          end
          ws.on(:close) do |event|
            events << 'close'
          end
        end
      end
    end

    @port = 9791
    q = Queue.new
    Thread.new do
      #$DEBUG=true
      #Thin::Logging.trace = true
      Thin::Logging.silent = true
      Rack::Handler.get('thin').run(app, :Port => @port) do |s|
        @server = s
        q.push nil
      end
    end
    q.pop
  end
  after do
    #$DEBUG=nil
    @server.stop
  end

  it "supports websocket requests" do
    ws = Faye::WebSocket::Client.new("ws://localhost:#{@port}")
    msg = nil
    sleep_for = 0.01
    wait_for = 10
    ws.on(:open){|event| msg = true}
    t = Time.now
    sleep sleep_for until msg || Time.now - t > wait_for
    msg.must_equal true

    msg = nil
    ws.on(:message){|event| msg = event.data}
    ws.send("hello")
    t = Time.now
    sleep sleep_for until msg || Time.now - t > wait_for
    msg.must_equal 'olleh'

    ws.close
    t = Time.now
    sleep sleep_for until @events == %w'open hello close' || Time.now - t > wait_for
    @events.must_equal %w'open hello close'
  end
end
end
end
