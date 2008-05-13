from __future__ import absolute_import

from logging import warning

def _matches( a, b ):
    matched = a == b or \
        isinstance(a,type) and \
        isinstance(b,a)
    # warning( "matches " + str(matched) + " " + str(a) + " : " + str(b) )
    return matched

class Gate(object):

    def __new__(cls):
        gate = Gate.Case()
        gate.accept( object )
        gate.accept( "actor" )
        gate.accept( "continuation" )
        gate.accept( "object" )
        return gate

    class Case(object):

        def __init__(self):
            self._always=[]
            self._list=[]

        def accept( self, *args ):
            return self.change( args, True, False )

        def change( self, args, value, inplace ):

            # warning( "change args: " + str( args ) + "/" + str(value) + " list: " + str(self._list) )
            
            prepend = True
            
            for list_index in xrange(len(self._list)):
                entry = self._list[list_index]
                vector, result, tag = entry
                matches = True
                if len(args) != len(vector):
                    matches = False
                else:
                    for arg_index in xrange(len(args)):
                        arg = args[arg_index]
                        if( vector[arg_index] != arg ):
                            matches = False
                            break
                if( matches ):
                    if( inplace ):
                        self._list[list_index][1] = value
                        prepend = False
                    else:
                        self._list.pop(list_index)
                    break
            if( prepend ):
                self._list.insert( 0, [ args, value, None ] )

            # warning( "changed: " + str(self._list) )
            
        def always( self, args, value, options = {} ):
            return self._change( self._always, list( args ), value, options )

        def _change( self, list, args, value, options = {} ):
            inplace = options.get( "inplace" )
            tag = options.get( "tag" )
            prepend = True
            tbd = []
            for list_index in xrange(len(list)):
                entry = list[list_index]
                vector, result, entry_tag = entry
                matches = True
                if( tag and entry_tag != tag ):
                    continue
                if( args ):
                    for arg_index in xrange(len(args)):
                        arg = args[arg_index]
                        if( arg_index >= len(vector) or
                            vector[arg_index] != arg ):
                            matches = False
                            break
                if( matches ):
                    if( inplace and value != None ):
                        list[list_index][1] = value
                        prepend = False
                    else:
                        tbd.append( list_index )
                        
            tbd.reverse()
            for index in tbd:
                list[index:index+1] = []
            if( prepend and value != None ):
                list.insert( 0, [ args, value, tag ] )

        def accepts( self, *args ):
            # warning( "accepts?? " + str(args) )
            if len(args) >= 3 and args[1] == "__getattribute__":
                args = args[0:1] + args[2:]
            accepted = False
            l = self._always + self._list
            for entry in l:
                vector, result, tag = entry
                matches = True
                for i in xrange(len(vector)):
                    v = vector[i]
                    if( not _matches( v, args[i] ) ):
                        matches = False
                        break
                if( matches ):
                    # warning( "does match " + str(entry) + " : " + str(args) )
                    accepted = result
                    break
                # warning( "does not match " + str(entry) + " : " + str(args) )
            # warning( "accepts? " + str(accepted) + " " + str(args) )
            return accepted

        def default_by_tag(self, tag):
            self._change( self._list, None, None, { "tag": tag } )

        def only( self, args, options = {} ):
            self._change( self._list, [ "object" ], False, options )
            self._change( self._list, [ "continuation" ], False, options )
            self._change( self._list, [ "continuation", object, "exception" ], True, options )
            self._change( self._list, list( args ), True, options )

        def list(self):
            return self._list

        def refuse(self, *args):
            return self.change( args, False, False )

        def default(self, args, options = {} ):
            return self._change( self._list, args, None, options )

'''
    def update value, *args
    end
  end

'''
