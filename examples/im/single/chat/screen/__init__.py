from __future__ import absolute_import

import chat.screen.wx

class Screen(object):

    def __new__( cls, *args, **kwds ):
        return chat.screen.wx.Screen( *args, **kwds )

