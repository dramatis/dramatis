require 'dramatis/actor'

class Kid

  Dramatis::Actor.acts_as self
  
  def initialize name, next_kid = nil
    @name = name
    @next = next_kid
  end

  def to_s
    @name
  end

  def whisper what
    what = mangle what
    puts "#{@name}: I heard #{what}"
    if @next
      @next.whisper what
    end
  end

  def mangle what
    what
  end

end
