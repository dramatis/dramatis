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
      var save = this.current_actor();
      this.current_actor_name = actor.__name__;
      actor.run();
      this.current_actor_name = save;
    },
    route: function(local) {
      return this.reactor().route(local);
    },
    current_continuation: function() {
      var continuation;
      var task = this.current_task();
      if (task) {
        continuation = task.continuation;
      }
      return continuation;
    },
    current_actor: function() {
      // FIX: create real default behavior class
      this.current_actor_name = this.current_actor_name || new Actor({"default":"behavior"});
      return this.current_actor_name;
    },
    current_task: function() {
      return this.current_actor().__runtime__.actor().current_task();
    },
    remove_current_continuation: function() {
      var continuation;
      var task = this.current_task();
      if (task) {
        continuation = task.remove_continuation();
      }
      return continuation;

    },
    register: function(id, actor) {
      if (!(actor instanceof Dramatis.Actor)) {
        actor = actor.__dramatis__ && actor.__dramatis__.actor;
      }
      if (!actor) {
        throw new Error("not an actor");
      }
      if (typeof id === "object" && id instanceof String) {
        id = id.toString();
      }
      if (typeof id === "number") {
        this.actor_map[id] = actor.__name__.__runtime__;
      } else {
        this.directory[id] = actor.__name__.__runtime__;
      }
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