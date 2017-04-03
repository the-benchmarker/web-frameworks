require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "backtracking_array plugin" do 
  it "backtracks to next entry in array if later matcher fails" do
    app(:backtracking_array) do |r|
      r.is %w'a a/b' do |id|
        id
      end

      r.is %w'c c/d', %w'd e' do |a, b|
        "#{a}-#{b}" 
      end

      r.is [%w'f f/g', %w'g g/h'] do |id|
        id
      end
    end

    tests = lambda do
      status.must_equal 404

      body("/a").must_equal 'a'
      body("/a/b").must_equal 'a/b'
      status("/a/b/").must_equal 404

      body("/c/d").must_equal 'c-d'
      body("/c/e").must_equal 'c-e'
      body("/c/d/d").must_equal 'c/d-d'
      body("/c/d/e").must_equal 'c/d-e'
      status("/c/d/").must_equal 404

      body("/f").must_equal 'f'
      body("/f/g").must_equal 'f/g'
      body("/g").must_equal 'g'
      body("/g/h").must_equal 'g/h'
      status("/f/g/").must_equal 404
      status("/g/h/").must_equal 404
    end

    tests.call
    app.plugin(:static_path_info)
    tests.call
  end
end
