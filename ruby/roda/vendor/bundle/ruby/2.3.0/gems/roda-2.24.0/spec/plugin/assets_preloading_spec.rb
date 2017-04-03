require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

run_tests = true
begin
  begin
    require 'tilt/sass'
  rescue LoadError
    begin
      for lib in %w'tilt sass'
        require lib
      end
    rescue LoadError
      warn "#{lib} not installed, skipping assets_preloading plugin test"
      run_tests = false
    end
  end
end

describe "assets_preloading plugin" do
  before do
    app(:bare) do
      plugin :assets, {
        :css => ['app.scss'],
        :js => { :head => ['app.js'] },
        :path => 'spec/assets',
        :public => 'spec',
      }
      plugin :assets_preloading

      route do |r|
        r.is 'header-css' do
          preload_assets_link_header :css
        end
        r.is 'header-js' do
          preload_assets_link_header [:js, :head]
        end
        r.is 'header-multiple' do
          preload_assets_link_header :css, [:js, :head]
        end

        r.is 'tags-css' do
          preload_assets_link_tags :css
        end
        r.is 'tags-js' do
          preload_assets_link_tags [:js, :head]
        end
        r.is 'tags-multiple' do
          preload_assets_link_tags :css, [:js, :head]
        end
      end
    end
  end

  it "preload_assets_link_header returns a well-formed header" do
    html = body('/header-multiple')

    assets = html.split(',')
    assets.count.must_equal 2

    assets.each do |asset|
      parts = asset.split(';')
      parts.count.must_equal 3

      parts[0].scan(/^<.*>$/).count.must_equal 1
      parts.select{|c| c == 'rel=preload' }.count.must_equal 1
      parts.select{|c| c.match(/^as=\w+$/) }.count.must_equal 1
    end
  end

  it "preload_assets_link_header returns the correct 'as' for an asset type" do
    html = body('/header-css')
    html.scan('as=style').count.must_equal 1

    html = body('/header-js')
    html.scan('as=script').count.must_equal 1
  end

  it "preload_assets_link_tags returns well-formed tags" do
    html = body('/tags-multiple')

    tags = html.scan(/<link.+?>/)
    tags.count.must_equal 2

    tags.each do |tag|
      tag.scan(' rel="preload"').count.must_equal 1
      tag.scan(/ href=".+"/).count.must_equal 1
      tag.scan(/ as=".+"/).count.must_equal 1
    end
  end

  it "preload_assets_link_tags returns the correct 'as' for an asset type" do
    html = body('/tags-css')
    html.scan('as="style"').count.must_equal 1

    html = body('/tags-js')
    html.scan('as="script"').count.must_equal 1
  end
end if run_tests
