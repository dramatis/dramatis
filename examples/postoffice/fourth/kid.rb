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
    @heard = mangle what
    @next and @next.whisper @heard
  end

  def ask
    @heard
  end

  def mangle what
    Mangler.mangle what
  end

end
