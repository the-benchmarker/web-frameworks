require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

begin
  require 'tilt/erb'
rescue LoadError
  warn "tilt not installed, skipping multi_view plugin test"  
else
describe "multi_view plugin" do 
  before do
    app(:bare) do
      plugin :render, :views=>'spec/views', :layout=>'layout-yield'
      plugin :multi_view

      route do |r|
        r.multi_view(['a', 'b', 'c'])
      end
    end
  end

  it "supports easy rendering of multiple views by name" do
    body('/a').gsub(/\s+/, '').must_equal "HeaderaFooter"
    body('/b').gsub(/\s+/, '').must_equal "HeaderbFooter"
    body('/c').gsub(/\s+/, '').must_equal "HeadercFooter"
    status('/d').must_equal 404
    status('/a', 'REQUEST_METHOD'=>'POST').must_equal 404
  end
end

describe "multi_view plugin multi_view_compile method " do 
  before do
    app(:bare) do
      plugin :render, :views=>'spec/views', :layout=>'layout-yield'
      plugin :multi_view
      regexp = multi_view_compile(['a', 'b', 'c'])

      route do |r|
        r.multi_view(regexp)
      end
    end
  end

  it "supports easy rendering of multiple views by name" do
    body('/a').gsub(/\s+/, '').must_equal "HeaderaFooter"
    body('/b').gsub(/\s+/, '').must_equal "HeaderbFooter"
    body('/c').gsub(/\s+/, '').must_equal "HeadercFooter"
    status('/d').must_equal 404
    status('/a', 'REQUEST_METHOD'=>'POST').must_equal 404
  end
end
end
