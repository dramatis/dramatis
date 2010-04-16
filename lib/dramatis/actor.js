"use strict";
(function(){
  var noJSON = function() {
    throw new Error("no automatic conversion of an actor to JSON");
  };
  var global = (function(){return this;}());
  var Actor = Dramatis.Actor =
    new Dramatis.Class(function Actor(behavior, name_class){
      var Cls = name_class || Actor.Name;
      this.id = parseInt((Math.random()+"").substring(2), 10);
      this.__name__ = new Cls(this,behavior);
      this.queue = [];
      this.become(behavior);
      // FIX: uuid
      this._fn_index = [];
      Actor.register(this,this.id);

      // this.publish("lifecycle", new Lifecycle(this));

      return this.__name__;
    }, [ Dramatis.Publisher ], {
      become: function become(behavior) {
        // FIX: detach old behavior
        this.behavior = behavior;
        if (behavior) {
          behavior.__dramatis__ =
            behavior.__dramatis__ || new Actor.Interface(this);
        }
      },
      name: function name(fn) {
        if (fn) { fn(this.__name__); }
        return this.__name__;
      },
      enqueue: function(task) {
        this.queue.push(task);
        if (this.queue.length === 1) {
          Dramatis.Director.current.run(this);
        }
      },
      current_task: function() {
        return this.current_task_;
      },
      run: function() {
        if (this.queue.length < 1) {
          throw new Error("unrunanble actor asked to run");
        }
        var save = this.current_task_;
        try {
          this.current_task_ = this.queue.shift();
          this.current_task_.call();
        } finally {
          this.current_task_ = save;
        }
        if (this.queue.length > 0) {
          Dramatis.Director.current.run(this);
        }
      },
      fn_index: function(fn) {
        if (!fn.__fn_index__) {
          fn.__fn_index__ = this._fn_index.length;
          this._fn_index.push(fn);
        }
        return fn.__fn_index__;
      },
      fn: function(index) {
        var fn;
        if (!(fn = this._fn_index[index])) {
          throw new Error("no fn at index "+index);
        }
        return fn;
      },
      terminate: function(reason) {
        var exception = reason;
        if (!exception) {
          exception = new Dramatis.Exception.Terminated.Normal("Normal");
        } else if (!(exception instanceof Dramatis.Exception.Terminated)) {
          exception = new Dramatis.Exception.Terminated.Other("Dramatis.Exception.Terminated: "+
                                                              reason); 
        }
        this.notify(this.__name__, exception);
        if (this.on_terminate) {
          this.on_terminate.apply([exception]);
        }
        Dramatis.Director.current.unregister(this);
        return exception;
      },
      abort: function(reason) {
        if (!reason) {
          reason = "unknown";
        }
        Dramatis.error("actor teminated: "+reason + " " + reason.stack);
        throw this.terminate(reason);
      },
      raise: function(exception) {
        this.abort(exception);
      }
    });

  Actor.register = function(actor, label) {
    Dramatis.Director.current.register(actor, label);
  };

  Actor.on_terminate = function(behavior, callable) {
    var runtime = behavior.__dramatis__.actor;
    runtime.on_terminate = new Dramatis.Runtime.Callable(runtime, callable);
  };

  Actor.terminate = function(/*...*/) {
    var actor = Array.prototype.shift.apply(arguments);
    var runtime =
      ( actor.__dramatis__ && actor.__dramatis__.actor.__name__.__runtime__ ) ||
      actor.__runtime__;
    runtime.actor_send("terminate", arguments);
  };

  Actor.id = function(object) {
    var name =
      ( object.__dramatis__ && object.__dramatis__.actor.__name__ ) || object;
    return name.__runtime__.id;
  };

  Actor.actor_name = function(object) {
    var name =
      ( object.__dramatis__ && object.__dramatis__.actor.__name__ ) || object;
    return name;
  };

  Actor.uri = function(object) {
    var name =
      ( object.__dramatis__ && object.__dramatis__.actor.__name__ ) || object;
    return Actor.Name.uri(name);
  };

  Actor.behavior = function(name) {
    var actor = name.__runtime__.actor && name.__runtime__.actor();
    return actor && actor.behavior;
  };

  var Lifecycle = new Dramatis.Class(function(actor){
    this.actor =
      ( actor.__dramatis__ && actor.__dramatis__.actor.__name__ ) || actor;
  }, {
    add_subscription: function(/*...*/) {
      // console.debug($.print(this.actor));
      this.actor.__runtime__.actor_send("add_subscription", arguments);
    },
    remove_subscription: function(/*...*/) {
      // console.debug($.print(this.actor));
      this.actor.__runtime__.actor_send("remove_subscription", arguments);
    }
  });

  Actor.lifecycle = function(actor) {
    return new Lifecycle(actor);
  };

  new Dramatis.Class.Subscope(Actor);
}());