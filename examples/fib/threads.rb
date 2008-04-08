#!/usr/bin/env ruby

# cf. http://gee.cs.oswego.edu/dl/papers/fj.pdf

# This variant doesn't produce as many threads and thus stresses
# the thread system less.

class Fib
  attr_accessor :value
  def initialize n, level = THREAD_LEVELS
    calc n, level
  end
  def calc n, level
    @value = if level == 0
               now = Time.now
               puts "#{Thread.current} start"
               v = sequential( n )
               puts "#{Thread.current} sequential(#{n}) #{Time.now-now}"
               v
             else
               if true
                 left = nil
                 left_thread = Thread.new { left = Fib.new( n - 1, level - 1 ) }
                 right = nil
                 right_thread = Thread.new { right = Fib.new( n - 2, level - 1 ) }
                 left_thread.join
                 right_thread.join
                 left.value + right.value
               else
                 sequential( n-1 ) + sequential( n-2 )
               end
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

n = begin ARGV.shift.to_i; rescue; end
n and n > 0 or n = 36

threads = begin ARGV.shift.to_i; rescue; end
threads and threads > 1 or threads = 1

THREAD_LEVELS = ( Math.log(threads)/Math.log(2) ).ceil

puts "fib(#{n}) = #{Fib.new(n).value}"
