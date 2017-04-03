require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "json_parser plugin" do 
  before do
    app(:json_parser) do |r|
      r.params['a']['b'].to_s
    end
  end

  it "parses incoming json if content type specifies json" do
    body('rack.input'=>StringIO.new('{"a":{"b":1}}'), 'CONTENT_TYPE'=>'text/json', 'REQUEST_METHOD'=>'POST').must_equal '1'
  end

  it "doesn't affect parsing of non-json content type" do
    body('rack.input'=>StringIO.new('a[b]=1'), 'REQUEST_METHOD'=>'POST').must_equal '1'
  end

  it "returns 400 for invalid json" do
    req('rack.input'=>StringIO.new('{"a":{"b":1}'), 'CONTENT_TYPE'=>'text/json', 'REQUEST_METHOD'=>'POST').must_equal [400, {}, []]
  end
end

describe "json_parser plugin" do 
  it "handles empty request bodies" do
    app(:json_parser) do |r|
      r.params.length.to_s
    end
    body('rack.input'=>StringIO.new(''), 'CONTENT_TYPE'=>'text/json', 'REQUEST_METHOD'=>'POST').must_equal '0'
  end

  it "supports :error_handler option" do
    app(:bare) do
      plugin(:json_parser, :error_handler=>proc{|r| r.halt [401, {}, ['bad']]})
      route do |r|
        r.params['a']['b'].to_s
      end
    end
    req('rack.input'=>StringIO.new('{"a":{"b":1}'), 'CONTENT_TYPE'=>'text/json', 'REQUEST_METHOD'=>'POST').must_equal [401, {}, ['bad']]
  end

  it "works with bare POST" do
    app(:bare) do
      plugin(:json_parser, :error_handler=>proc{|r| r.halt [401, {}, ['bad']]})
      route do |r|
        (r.POST['a']['b'] + r.POST['a']['c']).to_s
      end
    end
    body('rack.input'=>StringIO.new('{"a":{"b":1,"c":2}}'), 'CONTENT_TYPE'=>'text/json', 'REQUEST_METHOD'=>'POST').must_equal '3'
  end

  it "supports :parser option" do
    app(:bare) do
      plugin(:json_parser, :parser=>method(:eval))
      route do |r|
        r.params['a']['b'].to_s
      end
    end
    body('rack.input'=>StringIO.new("{'a'=>{'b'=>1}}"), 'CONTENT_TYPE'=>'text/json', 'REQUEST_METHOD'=>'POST').must_equal '1'
  end

  it "supports :include_request option" do
    app(:bare) do
      plugin(:json_parser,
        :include_request => true,
        :parser => lambda{|s,r| {'a'=>s, 'b'=>r.path_info}})
      route do |r|
        "#{r.params['a']}:#{r.params['b']}"
      end
    end
    body('rack.input'=>StringIO.new('{}'), 'CONTENT_TYPE'=>'text/json', 'REQUEST_METHOD'=>'POST').must_equal '{}:/'
  end
end
