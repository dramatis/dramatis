#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'pp'
a = [ :a, :b, :c ]
pp a
pp a[1,-1]
b = [ :a ]
pp b[1,b.length]
