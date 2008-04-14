#!/usr/bin/env ruby

# cf. http://gee.cs.oswego.edu/dl/papers/fj.pdf

# This variant doesn't produce as many threads and thus stresses
# the thread system less.

$:.push File.join( File.dirname(__FILE__), "..", "..", "lib" )

require 'dramatis/actor'

class Fib
  include Dramatis::Actor
  attr_accessor :value
  def initialize n, level = THREAD_LEVELS
    actor.refuse :value
    cast( actor.name ).calc n, level
  end
  def calc n, level
    @value = if level == 0
               now = Time.now
               puts "#{Thread.current} start"
               v = sequential( n )
               puts "#{Thread.current} sequential(#{n}) #{Time.now-now}"
               v
             else
               left = Fib.new( n - 1, level - 1 )
               right = Fib.new( n - 2, level - 1 )
               left.value + right.value
             end
    actor.accept :value
  end
  def sequential n
    if n <= 1 
      n
    else
      sequential(n-1) + sequential(n-2)
    end
  end
end

n = begin ARGV.shift.to_i; rescue; end
n and n > 0 or n = 36

threads = begin ARGV.shift.to_i; rescue; end
threads and threads > 1 or threads = 1

THREAD_LEVELS = ( Math.log(threads)/Math.log(2) ).ceil

puts "fib(#{n}) = #{Fib.new(n).value}"
