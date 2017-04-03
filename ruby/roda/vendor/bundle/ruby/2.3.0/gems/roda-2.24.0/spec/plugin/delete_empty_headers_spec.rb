require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "delete_empty_headers plugin" do 
  it "automatically deletes headers that are empty" do
    app(:delete_empty_headers) do |r|
      response['Foo'] = ''
      response['Content-Type'] = ''
      response['Content-Length'] = ''
      response['Bar'] = '1'
      'a'
    end

    req[1].must_equal('Bar'=>'1')
  end

  it "is called when finishing with a body" do
    app(:delete_empty_headers) do |r|
      response['Foo'] = ''
      response['Content-Type'] = ''
      response['Content-Length'] = ''
      response['Bar'] = '1'
      r.halt response.finish_with_body(['a'])
    end

    req[1].must_equal('Bar'=>'1')
  end
end
