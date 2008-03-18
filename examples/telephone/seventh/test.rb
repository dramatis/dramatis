smp = 10
o = Object.new
block = lambda { p smp; p "hi"; p self }
p "o", o.class
o.instance_eval do
  class << self
    @__bif__ = "foo"
  end
  # p @@__bif__ = "bif"
  p self.class_eval { @__bif__ }
  class << self
    p @__bif__
    p "<<", self
    def ____frob____
    end
  end
end
p "o", o.class
o.class.send :define_method, :bar, &block
o.instance_eval do
  def foo
    bar
  end
end

o.foo
o.instance_eval { bar }
Object.new.instance_eval { bar; p @__bif__ }
