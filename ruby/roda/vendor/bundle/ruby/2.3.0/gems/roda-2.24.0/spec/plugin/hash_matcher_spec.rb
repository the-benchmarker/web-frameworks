require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "hash_matcher plugin" do 
  it "should enable the handling of arbitrary hash keys" do
    app(:bare) do 
      plugin :hash_matcher
      hash_matcher(:foos){|v| consume(self.class.cached_matcher(:"foos-#{v}"){/((?:foo){#{v}})/})}
      route do |r|
        r.is :foos=>1 do |f|
          "1#{f}"
        end
        r.is :foos=>2 do |f|
          "2#{f}"
        end
        r.is :foos=>3 do |f|
          "3#{f}"
        end
      end
    end

    body("/foo").must_equal '1foo'
    body("/foofoo").must_equal '2foofoo'
    body("/foofoofoo").must_equal '3foofoofoo'
    status("/foofoofoofoo").must_equal 404
    status.must_equal 404
  end
end
