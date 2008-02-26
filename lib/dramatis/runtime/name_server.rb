module Dramatis; end
module Dramatis::Runtime; end

require 'dramatis/runtime/actor'

class Dramatis::Runtime::NameServer

  Actor = Dramatis::Runtime::Actor

  def self.the
    @the ||= self.new;
  end

  def initialize
    @table = {}
  end

  def new object
    if @table[object]
      raise "a snit"
    end
    actor =  Actor.new object
    @table[object] = actor if object
    actor
  end

  def [] object
    binding = @table[object]
    if !binding
      binding = Actor.new object
    end
    binding
  end

  def bind object, binding
    @table[object] = binding
  end

end
