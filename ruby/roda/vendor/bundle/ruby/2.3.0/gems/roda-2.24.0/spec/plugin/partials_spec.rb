require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

begin
  require 'tilt/erb'
rescue LoadError
  warn "tilt not installed, skipping partials plugin test"  
else
  describe "partials plugin" do
    before do
      app(:bare) do
        plugin :partials, :views=>"./spec/views"

        route do |r|
          r.is "partial" do
            partial("test", :locals=>{:title => "About Roda"})
          end

          r.is "partial/subdir" do
            partial("about/test", :locals=>{:title => "About Roda"})
          end

          r.is "partial/inline" do
            partial(:inline=>"Hello <%= name %>", :locals=>{:name => "Agent Smith"})
          end
        
        end
      end
    end

    it "partial renders without layout, and prepends _ to template" do
      body("/partial").strip.must_equal "<h1>About Roda</h1>"
    end

    it "partial renders without layout, and prepends _ to template" do
      body("/partial/subdir").strip.must_equal "<h1>Subdir: About Roda</h1>"
    end

    it "partial handles inline partials" do
      body("/partial/inline").strip.must_equal "Hello Agent Smith"
    end

  end
end
