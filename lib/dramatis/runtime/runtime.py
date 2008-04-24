class Runtime:

    _current = None

    @classmethod
    def current(cls):
        global _current
        cls._current = cls._current if cls._current else cls()
        return cls._current

    def quiesce(self):
        pass

    def exceptions(self):
        return ()

    @classmethod
    def reset(self):
        pass
