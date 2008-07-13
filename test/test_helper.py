import dramatis
import threading

class DramatisTestHelper(object):

    def runtime_check(self):
        try:
            dramatis.Runtime.current.quiesce()
            assert len( dramatis.Runtime.current.exceptions() ) == 0
            assert threading.activeCount() == \
                    1 + dramatis.runtime.Scheduler.current.thread_count
        finally:
            dramatis.Runtime.reset()
            assert threading.activeCount() == 1
        
