from __future__ import absolute_import

class Deadlock(Exception):
    """Raised when the runtime determines that deadlock has occurred.

    This mean there are no executing actors and while there are one or
    more tasks queued for one or more actors, all are gated off. Note
    that case where there are no tasks at all indicates normal
    termination."""
