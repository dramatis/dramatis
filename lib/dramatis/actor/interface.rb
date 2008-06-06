module Dramatis; end
module Dramatis::Actor; end

# A Dramatis::Actor::Interface object provides actors that have mixed
# in Dramatis::Actor with access to their actor name and other actor
# operations. An instance of Dramatis::Actor::Interface is typically accessed through
# Dramatis::Actor.actor.

# Many of the interface method affect the <em>gate behavior</em> of
# the actor, that is, whether tasks queued for the actor are allowed
# to execute. With functions refuse, accept, default, and always, an
# actor can control task scheduling.

# Most of these methods accept an array of arguments that are matched
# against each method by the runtime when determining whether a task
# can be scheduled.

# Each element in the array is tested, via ====, against the method
# and arguments of the task underconsideration. If all the arguments
# match, the pattern matches. Extra task parameters are ignored and
# the match succeeds. If there are more arguments in the pattern than
# there are associated with the task, the match fails.

# Note that the interaction of multiple calls is a bit complex and currently
# not documented. See the examples and tutorials.

# This object should only be accessed from the actor it represents.

class Dramatis::Actor::Interface

  # call-seq:
  #  refuse pattern_args -> nil
  #
  # Blocks the actor from running any tasks that match pattern_args. Note that
  # subsequent gate calls may override this behavior.

  def refuse *args
    @actor.gate.refuse( :object, *args )
  end

  # call-seq:
  #  accept pattern_args -> nil
  #
  # Enables the actor to run tasks that match pattern_args. Note that
  # subsequent gate calls may override this behavior.

  def accept *args
    @actor.gate.accept( :object, *args )
  end

  # call-seq:
  #  default pattern_args -> nil
  #
  # Reverts the behavior of the actor to tasks matching pattern_args
  # to the default. It un-does the affect of a call to refuse or
  # accept with the same arguments.

  def default *args
    @actor.gate.default( [ :object ] + args )
  end

  # call-seq:
  #  always( pattern_args, value ) -> nil
  #
  # Causes tasks matching pattern_args to always be accepted if +value+
  # is +true+ or reject if +value+ is +false+. always takes precendence over
  # refuse/accept so a task that matches both a refuse pattern and an
  # always( ..., true ) pattern will be allowed. always also overrides
  # the implict gating in rpc method calls.

  def always args, value
    @actor.gate.always( ( [ :object ] + Array( args ) ), value )
  end

  # call-seq:
  # enable_call_threading -> nil
  #
  # Enables call threading for actor method calls made by this
  # actor. When call threading is enabled, method gating is modified
  # such that recursive and co-recursive calls are allowed. Normally
  # blocking calls made by an actor on itself, e.g.,
  #   actor.name.some_method
  # would cause a deadlock. When call threading is enabled, recursion, both self-recursion
  # and co-recursion (actor A does an rpc on actor B which does an rpc on actor A), is allowed.

  def enable_call_threading
    @actor.enable_call_threading
    nil
  end

  # call-seq:
  #  name -> actor_name_of_actor
  #
  # Returns the actor name for the object.

  def name
    @actor.name
  end

  # call-seq:
  #  yield -> nil
  #
  # Yields the actor to allow other tasks to be executed.
  # Currently, messages are handled FIFO so the yield will
  # return when all the messages received up to the point of the
  # yield are executed. This could be modified if non-FIFO queue
  # processing is added

  def yield
    @actor.actor_send [ :yield ], :continuation => :rpc,
                                  :nonblocking => true
    nil
  end

  def timeout value, *args #:nodoc: not ready
    @actor.timeout value, *args
  end

  private

  def gate
    @actor.gate
  end

  def initialize actor #:nodoc:
    @actor = actor
  end

end
