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
      Dramatis.Director.current.register(this.id,this);
      return this.__name__;
    }, {
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
        this.current_task_ = this.queue.shift();
        this.current_task_.call();
        this.current_task_ = save;
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
      }
    });

  new Dramatis.Class.Subscope(Actor);
}());