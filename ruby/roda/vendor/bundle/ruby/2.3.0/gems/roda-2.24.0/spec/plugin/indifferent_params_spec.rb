require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "indifferent_params plugin" do 
  it "allows indifferent access to request params via params method" do
    app(:indifferent_params) do |r|
      r.on do
        "#{params[:a]}/#{params[:b][0][:c]}"
      end
    end

    body('QUERY_STRING'=>'a=2&b[][c]=3', 'rack.input'=>StringIO.new).must_equal '2/3'
    body('REQUEST_METHOD'=>'POST', 'rack.input'=>StringIO.new('a=2&b[][c]=3')).must_equal '2/3'
  end
end
