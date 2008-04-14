require 'dramatis/actor'

require 'mangler'

class Kid

  include Dramatis::Actor
  
  def initialize name, next_kid = nil
    @name = name
    @next = next_kid
  end

  def to_s
    @name
  end

  def whisper what
    mangle( @next.nil? && @next.whisper( mangle( what )  ) || what )
  end

  def mangle what
    Mangler.mangle what
  end

end
