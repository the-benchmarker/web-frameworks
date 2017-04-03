require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

begin
  require 'tilt/erb'
rescue LoadError
  warn "tilt not installed, skipping padrino_render plugin test"  
else
describe "padrino_render plugin" do
  before do
    app(:bare) do
      plugin :padrino_render, :views=>"./spec/views"

      route do |r|
        
        r.is "render" do
          render(:content=>'bar', :layout_opts=>{:locals=>{:title=>"Home"}})
        end

        r.is "render/nolayout" do
          render("about", :locals=>{:title => "No Layout"}, :layout=>nil)
        end
      end
    end
  end

  it "render uses layout by default" do
    body("/render").strip.must_equal "<title>Roda: Home</title>\nbar"
  end

  it "render doesn't use layout if layout is nil" do
    body("/render/nolayout").strip.must_equal "<h1>No Layout</h1>"
  end
end
end
