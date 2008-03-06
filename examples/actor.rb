#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor'

class Foo

  Dramatis::Actor.acts_as self

end

a = Foo.new
b = Foo.new
