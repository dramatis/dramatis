"use strict";

(function(){

  var Actor = Dramatis.Actor;
  var Name = Actor.Name;

  var Director = Dramatis.Director = function Director() {
    this.actor_map = {};
    this.directory = {};
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
    enqueue: function(task, options) {
      options = options || {};
      var name = task.callable.name;
      if (name instanceof Dramatis.Runtime.Actor.Name.Remote) {
        if (options.local_only) {
          throw new Dramatis.Exceptions.TaskEnqueueViolation();
        }
        this.reactor().send(name.route, task, task.continuation);
      } else {
        name.actor().enqueue(task);
      }
    },
    run: function(actor) {
      actor.run();
    },
    route: function(local) {
      return this.reactor().route(local);
    },
    current_actor: function() {
      this.current_actor_name = this.current_actor_name || new Actor({});
      return this.current_actor_name;
    },
    register: function(id, actor) {
      this.actor_map[id] = actor.__name__.__runtime__;
    },
    lookup: function(route, id) {
      var name;
      if (route.is_local(this.reactor())) {
        name = new Dramatis.Runtime.Actor.Name.Local([id]);
      }
      return name;
    },
    lookup_actor: function(id) {
      if (typeof id === "number") {
        return this.actor_map[id];
      } else {
        return this.directory[id];
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
      current.destroy();
      current = undefined;
    }
  };

}());