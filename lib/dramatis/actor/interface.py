class Interface(object):
    """provides actors with control over their runtime dynamics

    A dramatis.Actor.Interface object provides actors that have mixed
    in dramatis.Actor access to their actor name and other actor
    operations. An instance of dramatis.Actor.Interface is typically
    accessed through via self.actor.

    Many of the interface method affect the <em>gate behavior</em> of
    the actor, that is, whether tasks queued for the actor are allowed
    to execute. With functions refuse, accept, default, and always, an
    actor can control task scheduling.

    Most of these methods accept an array of arguments that are matched
    against each method by the runtime when determining whether a task
    can be scheduled.

    Each element in the array is tested, as "equal or subclass",
    against the method and arguments of the task
    underconsideration. If all the arguments match, the pattern
    matches. Extra task parameters are ignored and the match
    succeeds. If there are more arguments in the pattern than there
    are associated with the task, the match fails.

    Note that the interaction of multiple calls is a bit complex and currently
    not documented. See the examples and tutorials.

    This object should only be accessed from the actor it represents."""

    def __init__(self,actor):
        self._actor = actor

    def refuse( self, *args ):
        """Blocks the actor from running any tasks that match pattern_args.
        
        Note that subsequent gate calls may override this behavior."""

        self._actor._gate.refuse( "object", *args )

    def accept( self, *args ):
        """Enables the actor to run tasks that match pattern_args.

        Note that subsequent gate calls may override this behavior."""

        self._actor._gate.accept( "object", *args )

    def default( self, *args ):
        """Reverts the behavior of the actor to tasks matching pattern_args.
        
        Behavior is reverted to the default. It un-does the affect of
        a call to refuse or accept with the same arguments."""
        self._actor._gate.default( ( "object", ) + args )

    def always( self, args, value ):
        """Causes tasks matching pattern_args to always be accepted if +value+
        is +true+ or reject if +value+ is +false+.

        Always takes precendence over refuse/accept so a task that
        matches both a refuse pattern and an always( ..., true )
        pattern will be allowed. always also overrides the implict
        gating in rpc method calls."""
        if not isinstance( args, tuple ):
            args = ( args, )
        self._actor._gate.always( ( ( "object", ) + args ), value )

    def enable_call_threading( self ):
        """Enables call threading for actor method calls made by this actor.

        When call threading is enabled, method gating is modified such
        that recursive and co-recursive calls are allowed. Normally
        blocking calls made by an actor on itself, e.g.,

          actor.name.some_method

        would cause a deadlock. When call threading is enabled,
        recursion, both self-recursion and co-recursion (actor A does
        an rpc on actor B which does an rpc on actor A), is allowed."""
        self._actor._set_call_threading_enabled(True)

    @property
    def name( self ):
        "Returns the actor name for the object."
        return self._actor.name

    def actor_yield(self):
        """Yields the actor to allow other tasks to be executed.

        Currently, messages are handled FIFO so the yield will
        return when all the messages received up to the point of the
        yield are executed. This could be modified if non-FIFO queue
        processing is added."""
        self._actor.actor_send( [ "actor_yield" ], { "continuation": "rpc",
                                                     "nonblocking": True } )
        return None

    def _gate( self ):
        return self._actor._gate
