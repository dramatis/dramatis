#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

require 'pp'

cls = Class.new do
  Dramatis::Actor.acts_as self

  def rpc other
    warn "e"
    other.foo
    warn "f"
  end

end

a = cls.new
b = cls.new

begin
  a.rpc b
  raise "hell"
rescue NoMethodError
rescue Exception => e
  warn "!! #{e}"
  raise e
end

