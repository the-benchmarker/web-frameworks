require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "path_rewriter plugin" do 
  it "allows rewriting remaining path or PATH_INFO" do
    app(:bare) do
      plugin :path_rewriter
      rewrite_path '/1', '/a'
      rewrite_path '/a', '/b'
      rewrite_path '/c', '/d', :path_info=>true
      rewrite_path '/2', '/1', :path_info=>true
      rewrite_path '/3', '/h'
      rewrite_path '/3', '/g', :path_info=>true
      rewrite_path(/\A\/e\z/, '/f')
      rewrite_path(/\A\/(dynamic1)/){|match| "/#{match[1].capitalize}"}
      rewrite_path(/\A\/(dynamic2)/, :path_info=>true){|match| "/#{match[1].capitalize}"}
      proc{rewrite_path('/a', '/z'){|match| "/x"}}.must_raise(Roda::RodaError)
      proc{rewrite_path('/a', {:path_info=>true}, :path_info=>true)}.must_raise(Roda::RodaError)
      proc{rewrite_path('/a', {:path_info=>true}, :path_info=>true){|match| "/x"}}.must_raise(Roda::RodaError)

      route do |r|
        "#{r.path_info}:#{r.remaining_path}"
      end
    end

    body('/a').must_equal '/a:/b'
    body('/a/f').must_equal '/a/f:/b/f'
    body('/b').must_equal '/b:/b'
    body('/c').must_equal '/d:/d'
    body('/c/f').must_equal '/d/f:/d/f'
    body('/d').must_equal '/d:/d'
    body('/e').must_equal '/e:/f'
    body('/e/g').must_equal '/e/g:/e/g'
    body('/1').must_equal '/1:/b'
    body('/1/f').must_equal '/1/f:/b/f'
    body('/2').must_equal '/1:/b'
    body('/2/f').must_equal '/1/f:/b/f'
    body('/3').must_equal '/g:/g'
    body('/dynamic1').must_equal '/dynamic1:/Dynamic1'
    body('/dynamic2').must_equal '/Dynamic2:/Dynamic2'

    app.freeze
    body('/a').must_equal '/a:/b'
    proc{app.rewrite_path '/a', '/b'}.must_raise
  end
end
