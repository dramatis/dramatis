"use strict";

(function(){

  /*global Dramatis*/

  var Actor = Dramatis.Actor;
  var Name = Actor.Name;

  var Director = Dramatis.Director = function Director() {
  };

  Director.prototype = {
    connect: function connect(s, bad, good) { this.reactor().connect(s, bad, good);},
    disconnect: function disconnect(s) { this.reactor().disconnect(s); },

/*
    get reactor() {
      this._reactor = this._reactor || new Dramatis.Runtime.Reactor;
      return this._reactor;
    },
*/
    
    reactor: function reactor(fn) {
      this._reactor = this._reactor || new Dramatis.Runtime.Reactor();
      if (fn) {
          fn(this._reactor);
      }
      return this._reactor;
    },

    destory: function destory() {
      if (this._reactor) {
        this._reactor.destroy();
      }
    }

  };

  var current;

  Director.__defineGetter__("current", function(){
    current = current || new Director();
    return current;
  });

  Director.reset = function reset() {
    if( current ) {
      current.destory();
      current = undefined;
    }
  };

}());