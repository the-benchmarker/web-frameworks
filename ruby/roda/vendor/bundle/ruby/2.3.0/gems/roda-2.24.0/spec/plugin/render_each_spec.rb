require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

begin
  require 'tilt/erb'
rescue LoadError
  warn "tilt not installed, skipping render_each plugin test"  
else
describe "render_each plugin" do 
  it "calls render with each argument, returning joined string with all results" do
    app(:bare) do
      plugin :render_each
      def render_template(t, opts)
        "r#{t}#{opts[:locals][:foo] if opts[:locals]}#{opts[:bar]}#{opts[:locals][:bar] if opts[:locals]} "
      end 

      route do |r|
        r.root do
          render_each([1,2,3], :foo)
        end

        r.is 'a' do
          render_each([1,2,3], :bar, :local=>:foo, :bar=>4)
        end

        r.is 'b' do
          render_each([1,2,3], :bar, :local=>nil)
        end

        r.is 'c' do
          render_each([1,2,3], :bar, :locals=>{:foo=>4})
        end
      end
    end

    body.must_equal 'rfoo1 rfoo2 rfoo3 '
    body("/a").must_equal 'rbar14 rbar24 rbar34 '
    body("/b").must_equal 'rbar rbar rbar '
    body("/c").must_equal 'rbar41 rbar42 rbar43 '
  end
end
end
