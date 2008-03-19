#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

require 'pp'

class Foo
  Dramatis::Actor::acts_as self
  def foo arg
    raise "hell" if arg != :bar
    :foobar
  end
end

actor = Foo.new

result = nil
retval = ( Dramatis::Actor::Name( actor ).continue {|value| result = value } ).foo :bar

raise "hell: [#{retval.inspect}]" if retval != nil

Dramatis::Runtime.the.quiesce

raise "hell: [#{result.inspect}]" if result != :foobar
