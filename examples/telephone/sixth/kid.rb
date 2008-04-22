require 'dramatis/actor'
require 'dramatis/actor/name'

require 'mangler'

class Kid

  include Dramatis::Actor
  
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
    # puts "#{self} heard #{@heard}"
    if @next
      release( @next ).whisper @heard
    end
    actor.accept :ask
  end

  def ask
    actor.refuse :ask
    @heard
  end

  def mangle what
    Mangler.mangle what
  end

end
