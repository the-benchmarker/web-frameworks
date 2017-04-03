require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "not_allowed plugin" do 
  it "skips the current block if pass is called" do
    app(:not_allowed) do |r|
      r.get '' do
        'a'
      end

      r.is "c" do
        r.get do
          "cg"
        end

        r.post do
          "cp"
        end

        "c"
      end

      r.on "q" do
        r.is do
          r.get do
            "q"
          end
        end
      end

      r.get do
        r.is 'b' do
          'b'
        end
        r.is(/(d)/) do |s|
          s
        end
        r.get(/(e)/) do |s|
          s
        end
      end
    end

    body.must_equal 'a'
    status('REQUEST_METHOD'=>'POST').must_equal 405
    header('Allow', 'REQUEST_METHOD'=>'POST').must_equal 'GET'

    body('/b').must_equal 'b'
    status('/b', 'REQUEST_METHOD'=>'POST').must_equal 404

    body('/d').must_equal 'd'
    status('/d', 'REQUEST_METHOD'=>'POST').must_equal 404

    body('/e').must_equal 'e'
    status('/e', 'REQUEST_METHOD'=>'POST').must_equal 404

    body('/q').must_equal 'q'
    status('/q', 'REQUEST_METHOD'=>'POST').must_equal 405

    body('/c').must_equal 'cg'
    body('/c').must_equal 'cg'
    body('/c', 'REQUEST_METHOD'=>'POST').must_equal 'cp'
    body('/c', 'REQUEST_METHOD'=>'PATCH').must_equal 'c'
    status('/c', 'REQUEST_METHOD'=>'PATCH').must_equal 405
    header('Allow', '/c', 'REQUEST_METHOD'=>'PATCH').must_equal 'GET, POST'

    @app.plugin :head
    header('Allow', '/c', 'REQUEST_METHOD'=>'PATCH').must_equal 'HEAD, GET, POST'
  end
end
