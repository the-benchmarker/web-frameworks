require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "module_include plugin" do 
  it "must_include given module in request or response class" do
    app(:bare) do
      plugin :module_include
      request_module(Module.new{def h; halt response.finish end})
      response_module(Module.new{def finish; [1, {}, []] end})

      route do |r|
        r.h
      end
    end

    req.must_equal [1, {}, []]
  end

  it "should accept blocks and turn them into modules" do
    app(:bare) do
      plugin :module_include
      request_module{def h; halt response.finish end}
      response_module{def finish; [1, {}, []] end}

      route do |r|
        r.h
      end
    end

    req.must_equal [1, {}, []]
  end

  it "should work if called multiple times with a block" do
    app(:bare) do
      plugin :module_include
      request_module{def h; halt response.f end}
      request_module{def i; h end}
      response_module{def f; finish end}
      response_module{def finish; [1, {}, []] end}

      route do |r|
        r.i
      end
    end

    req.must_equal [1, {}, []]
  end

end
