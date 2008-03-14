#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "..", "lib" )

require 'kid'
require 'dramatis/shoes'

tom = Kid.new "Tom"
dick = Kid.new "Dick", tom
harry = Kid.new "Harry", dick

shoes = Dramatis::Shoes.runtime

shoes.show tom, dick, harry

harry.whisper "foo"
