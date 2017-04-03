require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "match_affix plugin" do 
  it "allows changing the match prefix/suffix" do
    app(:bare) do
      plugin :match_affix, "", /(\/|\z)/

      route do |r|
        r.on "/albums" do |b|
          r.on "b/:id" do |id, s|
            "b-#{b}-#{id}-#{s.inspect}"
          end

          "albums-#{b}"
        end
      end
    end

    body("/albums/a/1").must_equal 'albums-/'
    body("/albums/b/1").must_equal 'b-/-1-""'
  end

  it "handles extra trailing slash only" do
    app(:bare) do
      plugin :match_affix, nil, /(?:\/\z|(?=\/|\z))/

      route do |r|
        r.on "albums" do
          r.on "b" do
            "albums/b:#{r.remaining_path}"
          end

          "albums:#{r.remaining_path}"
        end
      end
    end

    body("/albums/a").must_equal 'albums:/a'
    body("/albums/a/").must_equal 'albums:/a/'
    body("/albums/b").must_equal 'albums/b:'
    body("/albums/b/").must_equal 'albums/b:'
  end
end
