require 'dramatis/actor'

require 'mangler'

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
    if @next
      @next.whisper what
    else
      puts "#{@name}: #{what}"
    end
  end

  def mangle what
    Mangler.mangle what
  end

end
