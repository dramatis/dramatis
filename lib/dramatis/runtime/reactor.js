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
      destroy: function destroy() {}
    });
  (new Dramatis.Class.Subscope(Reactor));
}());
