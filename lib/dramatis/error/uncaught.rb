module Dramatis; end

class Dramatis::Error < StandardError; end

# Exception raised when the runtime exits with uncaught exceptions.

class Dramatis::Error::Uncaught < Dramatis::Error

  # call-seq:
  #   to_s -> string
  # 
  # combines the name of all uncaught exceptions.

  def to_s
    super + ": " + @exceptions.join( " "  )
  end

  def initialize exceptions #:nodoc:
    @exceptions = exceptions
  end

end
