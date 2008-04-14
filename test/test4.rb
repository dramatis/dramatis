#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

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
retval = ( dramatis( actor ).continue {|value| result = value } ).foo :bar

raise "hell: [#{retval.inspect}]" if retval != nil

Dramatis::Runtime.the.quiesce

raise "hell: [#{result.inspect}]" if result != :foobar
