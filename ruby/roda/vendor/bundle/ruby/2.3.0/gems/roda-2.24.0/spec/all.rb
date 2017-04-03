$:.unshift(File.expand_path("../lib", File.dirname(__FILE__)))
require 'rubygems'
(Dir['./spec/*_spec.rb'] + Dir['./spec/plugin/*_spec.rb']).each{|f| require f}
