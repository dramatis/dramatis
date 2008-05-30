#!/usr/bin/env python

import inspect
import sys
import os.path

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..', '..', '..', 'lib' ) ]

from logging import warning

from chat.screen.wx import Screen
from chat import Server
from chat import Client

password = "xyzzy"
group = "general"

screen = Screen()

print repr(screen)

server = Server( password )

joe = Client( screen, server, password, group, "joe" )
jane = Client( screen, server, password, group, "jane" )
jim = Client( screen, server, password, group, "jim" )
sue = Client( screen, server, password, group, "sue" )

