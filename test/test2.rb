#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'

include Dramatis

name = Dramatis::Actor.new

interface( name ).bind Object.new

