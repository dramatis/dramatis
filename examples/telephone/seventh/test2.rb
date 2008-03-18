o = Object.new

smp = 10
block = lambda { p smp }
block.call

( class << o; self; end ).send :define_method, :foo, lambda { p "yo!" }

o.foo
Object.new.foo
