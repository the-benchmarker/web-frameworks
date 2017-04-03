require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "delegate plugin" do 
  it "adds request_delegate and response_delegate class methods for delegating" do
    app(:bare) do 
      plugin :delegate
      request_delegate :root
      response_delegate :headers

      def self.a; 'foo'; end
      class_delegate :a

      route do
        root do
          headers['Content-Type'] = a
        end
      end
    end
    
    header('Content-Type').must_equal 'foo'
    status('/foo').must_equal 404
  end
end
