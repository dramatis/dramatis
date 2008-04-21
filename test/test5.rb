#!/bin/env ruby
require 'pp'
def foo *args
  pp args
end
args = [ :a, :b, :c ]
foo( *args )
p foo
