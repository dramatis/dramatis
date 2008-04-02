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

phrases = [ "his mom locked her keys in the car, " +
            "so he should get a ride home with Hector",
            "Mac King is a comedy magic genius" ]

phrases.each do |phrase|
  puts "Teacher: #{phrase}"
  begin
    sally.whisper phrase
  rescue Dramatis::Deadlock
    warn "woah: got a deadlock: that shouln't happen"
  end
end

phrases.length.times do
  puts "Teacher heard: '#{tom.ask}'"
end
