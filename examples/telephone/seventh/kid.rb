require 'dramatis/actor'
require 'dramatis/actor/name'

require 'mangler'

class Kid

  Dramatis::Actor.acts_as self
  Actor = Dramatis::Actor
  
  def initialize name, next_kid = nil
    @name = name
    @next = next_kid
    @heard = "I ain't heard nuthin"
    actor.refuse :ask
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
    actor.accept :ask
    actor.refuse :whisper
  end

  def ask
    actor.refuse :ask
    actor.accept :whisper
    @heard
  end

  def mangle what
    Mangler.mangle what
  end

end
