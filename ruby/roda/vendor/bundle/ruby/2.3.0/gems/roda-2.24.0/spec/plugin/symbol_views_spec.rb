require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "symbol_views plugin" do
  before do
    app(:bare) do
      plugin :symbol_views

      def view(s)
        "v#{s}"
      end

      route do |r|
        r.root do
          :sym
        end

        r.is "string" do
          'string'
        end
      end
    end
  end

  it "should call view with the symbol" do
    body.must_equal "vsym"
  end

  it "should not affect other return types" do
    body("/string").must_equal 'string'
    body("/foo").must_equal ''
  end
end
