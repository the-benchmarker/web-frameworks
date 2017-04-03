require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "symbol_status plugin" do
  it "accepts a symbol" do
    app(:symbol_status) do |r|
      r.on do
        response.status = :unauthorized
        nil
      end
    end

    status.must_equal 401
  end

  it "accepts a fixnum" do
    app(:symbol_status) do |r|
      r.on do
        response.status = 204
        nil
      end
    end

    status.must_equal 204
  end
end
