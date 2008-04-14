#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "..", "..", "lib" )

require 'dramatis/actor'
require 'chat'

server = Chat::Server.new ARGV

puts dramatis( server ).url

