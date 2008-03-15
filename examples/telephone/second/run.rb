#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "..", "..", "lib" )
$:.push File.join( File.dirname(__FILE__), ".." )

require 'kid'

tom = Kid.new "Tom"
becky = Kid.new "Becky", tom
dick = Kid.new "Dick", becky
jane = Kid.new "Jane", dick
harry = Kid.new "Harry", jane
sally = Kid.new "Sally", harry

phrase = "his mom locked her keys in the car, " +
         "so he should get a ride home with Hector"

puts "Teacher: #{phrase}"
heard = sally.whisper phrase
puts "Teacher heard: #{heard}"
