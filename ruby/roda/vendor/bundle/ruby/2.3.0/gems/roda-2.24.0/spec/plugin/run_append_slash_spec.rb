require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "run_append_slash plugin" do
  before do
    sub2 = app do |r|
      r.root do
        'sub-bar-root'
      end

      r.get 'baz' do
        'sub-bar-baz'
      end
    end

    sub1 = app(:run_append_slash) do |r|
      r.root do
        'sub-root'
      end

      r.get 'foo' do
        'sub-foo'
      end

      r.on 'bar' do
        r.run sub2
      end
    end

    app(:bare) do
      route do |r|
        r.root do
          'root'
        end

        r.on 'sub' do
          r.run sub1
        end
      end
    end
  end

  it "internally appends a missing trailing slash to #run sub apps" do
    # Without append slash
    body.must_equal 'root'
    status('/sub').must_equal 404
    body('/sub/').must_equal 'sub-root'
    body('/sub/foo').must_equal 'sub-foo'
    status('/sub/foo/').must_equal 404
    body('/sub/bar/').must_equal 'sub-bar-root'
    body('/sub/bar/baz').must_equal 'sub-bar-baz'
    status('/sub/bar/baz/').must_equal 404

    # With append slash
    app.plugin :run_append_slash
    body('/sub').must_equal 'sub-root'
    body('/sub/').must_equal 'sub-root'
    body('/sub/foo').must_equal 'sub-foo'
    status('/sub/foo/').must_equal 404
    body('/sub/bar').must_equal 'sub-bar-root'
    body('/sub/bar/').must_equal 'sub-bar-root'
    body('/sub/bar/baz').must_equal 'sub-bar-baz'
    status('/sub/bar/baz/').must_equal 404
  end

  it "redirects #run sub apps when trailing slash is missing" do
    app.plugin :run_append_slash, :use_redirects => true
    status('/sub').must_equal 302
    header('Location', '/sub').must_equal '/sub/'
    body('/sub/').must_equal 'sub-root'
    body('/sub/foo').must_equal 'sub-foo'
    status('/sub/foo/').must_equal 404
    body('/sub/bar').must_equal 'sub-bar-root'
    body('/sub/bar/').must_equal 'sub-bar-root'
    body('/sub/bar/baz').must_equal 'sub-bar-baz'
    status('/sub/bar/baz/').must_equal 404
  end
end
