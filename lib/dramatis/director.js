"use strict";

(function(){

  var Actor = Dramatis.Actor;
  var Name = Actor.Name;

  var Director = Dramatis.Director = function Director() {
  };

  Director.prototype = {
    connect: function connect(s, bad, good) {
      this.reactor().connect(s, bad, good);
    },
    disconnect: function disconnect(s) {
      this.reactor().disconnect(s);
    },
    reactor: function reactor(fn) {
      this._reactor = this._reactor || new Dramatis.Runtime.Reactor();
      if (fn) {
          fn(this._reactor);
      }
      return this._reactor;
    },
    destroy: function destroy() {
      if (this._reactor) {
        this._reactor.destroy();
      }
    },
    enqueue: function(task) {
      var name = task.name;
      if (name instanceof Dramatis.Runtime.Actor.Name.Remote) {
        this.reactor().send(name.route, task);
      } else {
        name.actor.enqueue(task);
      }
    },
    run: function(actor) {
      actor.run();
    },
    route: function(local) {
      return this.reactor().route(local);
    }
  };

  var current;

  Director.__defineGetter__("current", function(){
    current = current || new Director();
    return current;
  });

  Director.reset = function reset() {
    if( current ) {
      current.destroy();
      current = undefined;
    }
  };

}());