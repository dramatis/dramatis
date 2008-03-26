#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'

name = Dramatis::Actor.new

Dramatis::Actor::Name( name ).bind Object.new

