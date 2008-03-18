require 'dramatis/actor/name'
require 'thread'
require 'pp'

begin
  Dramatis::Actor.new.foo
rescue Exception => e
  puts "got expected #{e}"
end
pp Thread.list
Dramatis::Runtime.the.quiesce
pp Thread.list

