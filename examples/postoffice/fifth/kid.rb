require 'dramatis/actor'
require 'dramatis/actor/name'

require 'mangler'

class Kid

  Dramatis::Actor.acts_as self
  Actor = Dramatis::Actor
  
  def initialize name, next_kid = nil
    @name = name
    @next = next_kid
  end

  def to_s
    @name
  end

  def whisper what
    @heard = mangle what
    if @next
      next_kid = Actor::Name( @next ).continue nil
      next_kid.whisper @heard
    end
  end

  def ask
    @heard
  end

  def mangle what
    Mangler.mangle what
  end

end
