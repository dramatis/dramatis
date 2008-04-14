#!/usr/bin/env ruby

# cf. http://gee.cs.oswego.edu/dl/papers/fj.pdf

$:.push File.join( File.dirname(__FILE__), "..", "..", "lib" )

require 'dramatis/actor'

class Fib
  include Dramatis::Actor
  THRESHOLD = 13
  attr_accessor :value
  def initialize n
    @value = if n <= THRESHOLD
               sequential( n )
             else
               left = Fib.new( n - 1 )
               right = Fib.new( n - 2 )
               left.value + right.value
             end
  end
  def sequential n
    if n <= 1 
      n
    else
      sequential(n-1) + sequential(n-2)
    end
  end
end

n = 28

puts "fib(#{n}) = #{Fib.new(n).value}"
