class App
  class << self
    def call env
      begin
        process env
      rescue Exception => e
        handle_error e
      end
    end

    def process env
      request = Request.new( env )
      request.response.finish
    end

    def handle_error e
      puts "Error processing request: #{ e.message }"
      puts e.backtrace[0..6]

      Rack::Response.new( 'Error', 500 ).finish
    end
  end
end