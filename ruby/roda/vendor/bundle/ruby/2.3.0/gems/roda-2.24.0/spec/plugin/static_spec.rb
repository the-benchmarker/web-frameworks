require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "static plugin" do 
  it "adds support for serving static files" do
    app(:bare) do
      plugin :static, ['/about'], :root=>'spec/views'

      route do
        'a'
      end
    end

    body.must_equal 'a'
    body('/about/_test.erb').must_equal File.read('spec/views/about/_test.erb')
  end

  it "respects the application's :root option" do
    app(:bare) do
      opts[:root] = File.expand_path('../../', __FILE__)
      plugin :static, ['/about'], :root=>'views'

      route do
        'a'
      end
    end

    body.must_equal 'a'
    body('/about/_test.erb').must_equal File.read('spec/views/about/_test.erb')
  end
end
