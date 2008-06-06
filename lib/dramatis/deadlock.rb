module Dramatis; end

class Dramatis::Error < StandardError; end

# Exception raised when the runtime determines that deadlock has
# occurred: that is, there are no executing actors and while
# there are one or more tasks queued for one or more actors, all are
# gated off. Note that case where there are no tasks at all indicates
# normal termination.

class Dramatis::Deadlock < Dramatis::Error; end
