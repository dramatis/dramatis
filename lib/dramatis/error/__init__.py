from __future__ import absolute_import

import re

from sys import exc_info

from traceback import extract_tb
from traceback import extract_stack
from traceback import format_list

class Error(Exception): pass

class Interface(Error): pass

class Uncaught(Error): pass

class Bind(Error): pass

class Internal(Error): pass

_re_dramatis = re.compile( r'/lib/dramatis/' )
_re_dramatis_an = re.compile( r'/lib/dramatis/actor/name/' )
_re_deadlock = re.compile( r'/lib/dramatis/deadlock.py' )
_re_threading = re.compile( r'/threading.py$' )
_re_traceback = re.compile( r'/lib/dramatis/error/' )

class Traceback(object):

    @property
    def traceback(self): return self._traceback

    def __init__( self, next  ):
        self._next = next
        if next:
            self._raw_traceback = next._raw_traceback
        else:
            self._raw_traceback = []

    def __str__(self):
        return "".join(format_list( self._traceback ))

    def __getitem__(self, index):
        return self._traceback[index]


    def set(self,traceback = None):
        if traceback:
            # warning( "foo " + "".join(format_list(extract_stack())))
            # print "there"
            self._raw_traceback = extract_tb(traceback) + self._raw_traceback
            # warning( "comb" )
            # warning( "".join( format_list(extract_tb(traceback)) ) )
            # warning( "comb here" )
            # warning( "".join( format_list(extract_stack())) )
            # warning( "comb done" )
        else:
            # warning( "none" )
            # warning( "".join( format_list(extract_stack())) )
            # warning( "none done")
            self._raw_traceback = extract_stack() + self._raw_traceback
            
        array = list(self._raw_traceback)
        array.reverse()

        # remove the scheduler

        # warning( "b4" + "".join(format_list( array )) )

        # print "START"

        filtered = []
        for v in array:
            file, line, func = v[0:3]
            # print file, line, func
            if not _re_dramatis.search(file):
                # print "keep"
                filtered.append( v )
                continue
            if _re_traceback.search(file) and func == "set":
                # func =~ %r{\Wmaybe_deadlock\W} and next
                # print "skip"
                continue
            if func == "_run":
                # print "stop"
                break
            # print "v", v
            # print "keep"
            filtered.append( v )
    
        # print "AGAIN"

        # remove queueing delivery

        array = filtered
        filtered = []
        skipping = False
        for v in array:

            file, line, func = v[0:3]

            # print file,line,func
            
            if skipping and (
                ( _re_threading.search(file) and ( ( func == "run" ) or
                                                             ( func == "__bootstrap_inner" ) or
                                                             ( func == "__bootstrap" ) ) )
                # or ( file =~ %r{/runtime/actor} and func =~ %r{\Wsend\W} ) \
                    ):
                # print "continue skipping"
                continue

            if not _re_dramatis.search(file):
                if skipping:
                    # print "keep no skip"
                    pass
                else:
                    # print "keep"
                    pass
                skipping = False
                filtered.append( v )
                continue
                
            if not skipping and (
                ( func == "queued" ) or
                ( func == "deliver" )
                # or ( file =~ %r{/runtime/actor} and func =~ %r{\Wsend\W} ) \
                    ):
                # print "skip skipping"
                skipping = True
                continue

            if _re_dramatis_an.search(file) and func == "__call__":
                # print "keep no skip"
                skipping = False
                continue

            skipping or filtered.append( v )

        # print "filt", "".join(format_list(filtered))

        self._traceback = filtered
        self._traceback.reverse()

def traceback(exception,next = None):
    tb = exception.__dict__.get( "_dramatis_traeback" )
    if not tb:
        tb = exception.__dict__["_dramatis_traeback"] = Traceback(next)
    return tb

def exception(e):
    tb = traceback(e)
    tb.set(exc_info()[2])
    return tb
