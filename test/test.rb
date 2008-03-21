#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'thread'
require 'pp'

begin
  Dramatis::Actor.new.foo
rescue Dramatis::Deadlock => e
  puts "got expected #{e}"
end
# pp Thread.list
Dramatis::Runtime.the.quiesce
# pp Thread.list
raise "hell" if Thread.list.length > 1

