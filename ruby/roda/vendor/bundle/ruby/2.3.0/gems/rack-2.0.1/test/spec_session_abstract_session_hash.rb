require 'minitest/autorun'
require 'rack/session/abstract/id'

describe Rack::Session::Abstract::SessionHash do
  attr_reader :hash

  def setup
    super
    store = Class.new do
      def load_session(req)
        ["id", {foo: :bar, baz: :qux}]
      end
      def session_exists?(req)
        true
      end
    end
    @hash = Rack::Session::Abstract::SessionHash.new(store.new, nil)
  end

  it "returns keys" do
    assert_equal ["foo", "baz"], hash.keys
  end

  it "returns values" do
    assert_equal [:bar, :qux], hash.values
  end

end
