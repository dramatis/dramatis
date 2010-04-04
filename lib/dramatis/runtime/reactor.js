"use strict";
/*global Dramatis*/
(function(){
  var Runtime = Dramatis.Runtime;
  var Reactor = Runtime.Reactor =
    new Dramatis.Runtime.Class(function Reactor(){
      this.connections = [];
    },{

      connect: function connect(url,good,bad) {
        var self = this;
        (new Reactor.Channel(url, function connection_succeeded(channel) {
          self.connections.push(channel);
          if (good) {
            good(channel);
          }
        }, function connection_failed(reason) {
          if (bad) {
            bad(reason);
          }
        }));
      },

      disconnect: function disconnect() {},
      destroy: function destroy() {},

      send: function(route, object, continuation) {
        var i;
        for(i = 0; i < this.connections.length; i++) {
          if (route instanceof this.connections[i].route.constructor) {
            return this.connections[i].send(route, object, continuation);
          }
        }
        throw new Error("could not route to "+route);
      },
      route: function(local) {
        // FIX: ...
        return this.connections[0].route;
      },
      route_is_local: function(route) {
        for(var i=0; i < this.connections.length; i++) {
          if (this.connections[i].route.equals(route)) {
            return true;
          }
        }
        return false;
      }
    });
  (new Dramatis.Class.Subscope(Reactor));

  Reactor.parsers = [];

  Reactor.register_parser = function(parser) {
    this.parsers.push(parser);
  };
  
  Reactor.parse_url = function(/*...*/) {
    for(var i=0; i < this.parsers.length; i++) {
      var parsed = this.parsers[i].apply(this.parsers[i],arguments);
      if (parsed) {
        return parsed;
      }
    }
    var nil;
    return nil;
  };

}());
