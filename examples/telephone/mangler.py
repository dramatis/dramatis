import re
import random

import inspect
import sys
import os.path

class Mangler(object):
    
    @classmethod
    def mangle_word(cls,what):
        # print "what", what
        patterns = [ what, what + "." ]
        for index in xrange(len(what)):

            if len(what) > 3:
                mod = what
                mod = mod[0:index]+mod[index+1:]
                patterns.append(mod)

            mod = what
            mod = mod[0:index] + "." + mod[index:]
            patterns.append(mod)

            if len(what) > 3:
                mod = what
                mod = mod[0:index] + "." + mod[index+1:]
                patterns.append(mod)

        # print repr(patterns)

        patterns = "^((" + ")|(".join(patterns) + "))$"
        regexp = re.compile( patterns, re.IGNORECASE )
        matches = [ word for word in cls._words() if regexp.match( word ) ]
        # print "a", repr(matches)
        # print "b", repr(patterns)
        # print "c", repr(matches)
        return matches[random.randint( 0, len(matches)-1 )]

    @classmethod
    def mangle(cls,words):
        return " ".join( [ cls.mangle_word( word ) for word in words.split() ] )

    @classmethod
    def _words(cls):
        try:
            return cls._word_list
        except:
            f = os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), "3esl.txt" )
            f = open(f)
            cls._word_list = [ l[0:-1] for l in f ]
            return cls._word_list
