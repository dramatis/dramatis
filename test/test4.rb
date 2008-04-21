#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

include Dramatis

require 'pp'

class Foo
  include Dramatis::Actor
  def foo arg
    raise "hell" if arg != :bar
    :foobar
  end
end

actor = Foo.new

result = nil
retval = ( interface( actor ).continue {|value| result = value } ).foo :bar

raise "hell: [#{retval.inspect}]" if retval != nil

Dramatis::Runtime.current.quiesce

raise "hell: [#{result.inspect}]" if result != :foobar
