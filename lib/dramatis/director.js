"use strict";

(function(){

  var Actor = Dramatis.Actor;
  var Name = Actor.Name;

  var Director = Dramatis.Director = function Director() {
    this.actor_map = {};
    this.directory = {};
    this.directory_index = {};
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
      var continuation = task.continuation;
      try {
        options = options || {};
        var name = task.callable.name;
        if (name instanceof Dramatis.Runtime.Actor.Name.Remote) {
          if (options.local_only) {
            throw new Dramatis.Exception.TaskEnqueueViolation();
          }
          this.reactor().send(name.route, task, continuation);
        } else {
          name.actor().enqueue(task);
        }
      } catch(e) {
        // Hmmm ...
        // console.debug("GE", e,this.current_task(), task);
        if (false) {
          throw e;
        }
        if (false && (!continuation || !continuation.exception)) {
          task = this.current_task();
          if (task) {
            continuation = task.continuation;
          }
        }
        if (continuation && continuation.exception) {
          continuation.exception.call(e);
        } else {
          // window.backtrace();
          // Dramatis.error("uncaught exception: "+e+" ("+e.constructor+") "+e.stack);
          throw e;
        }
      }
    },
    run: function(actor) {
      var save = this.current_actor();
      try {
        this.current_actor_name = actor.__name__;
        actor.run();
      } finally {
        this.current_actor_name = save;
      }
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
    raise: function(e) {
      this.current_task().raise(e);
    },
    remove_current_continuation: function() {
      var continuation;
      var task = this.current_task();
      if (task) {
        continuation = task.remove_continuation();
      }
      return continuation;

    },
    unregister: function(actor) {
      var id = actor.__name__.id;
      delete this.actor_map[id];
      for(var key in this.directory_index[id]) {
        delete this.directory[key];
      }
      delete this.directory_index[id];
    },
    register: function(actor, id) {
      if (!(actor instanceof Dramatis.Actor)) {
        actor = actor.__dramatis__ && actor.__dramatis__.actor;
      }
      if (!actor) {
        throw new Error("not an actor: "+actor);
      }
      if (typeof id === "object" && id instanceof String) {
        id = id.toString();
      }
      if (typeof id === "number") {
        this.actor_map[id] = actor.__name__.__runtime__;
      } else {
        this.directory[id] = actor.__name__.__runtime__;
        var actor_id = actor.__name__.__runtime__.id;
        this.directory_index[actor_id] = this.directory_index[actor_id] || {};
        this.directory_index[actor_id][id] = true;
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