require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

describe "shared_vars plugin" do 
  it "adds shared method for sharing variables across multiple apps" do
    app(:shared_vars) {|r| shared[:c]}
    old_app = app
    app(:shared_vars) do |r|
      shared[:c] = 'c'
      r.run old_app
    end

    body.must_equal 'c'
  end

  it "adds shared with hash merges the hash into the shared vars" do
    app(:shared_vars) do |r|
      shared(:c=>'c')
      shared[:c]
    end

    body.must_equal 'c'
  end

  it "calling shared with hash and a block sets shared variables only for that block" do
    app(:shared_vars) do |r|
      c = nil
      d = nil
      shared[:c] = 'b'
      shared(:c=>'c', :d=>'d') do
        c = shared[:c]
        d = shared[:d]
      end
      "#{shared[:c]}:#{shared[:d]}:#{c}:#{d}"
    end

    body.must_equal 'b::c:d'
  end

  it "calling shared with no arguments and a block raises an error" do
    app(:shared_vars) do |r|
      shared{}
    end
    proc{body}.must_raise(Roda::RodaError)
  end
end
