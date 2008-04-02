#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "..", "..", "lib" )

require 'chat'

screen = Chat::Screen.new

Chat::Client.new screen, *ARGV
