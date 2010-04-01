"use strict";
(function(){
  var Actor = Dramatis.Actor =
    new Dramatis.Class(function Actor(behavior, name_class){
      var Cls = name_class || Actor.Name;
      this.__name__ = new Cls(this,behavior);
      this.queue = [];
      this.become(behavior);
      return this.__name__;
    }, {
      become: function become(behavior) {
        // FIX: detach old behavior
        this.behavior = behavior;
        if(behavior){
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
      run: function() {
        if (this.queue.length < 1) {
          throw new Error("unrunanble actor asked to run");
        }
        var task = this.queue.shift();
        task.call();
        if (this.queue.length > 0) {
          Dramatis.Director.current.run(this);
        }
      }
    });

  new Dramatis.Class.Subscope(Actor);
}());