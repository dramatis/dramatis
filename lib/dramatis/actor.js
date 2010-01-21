"use strict";
/*global Dramatis*/
(function(){

  var Actor = Dramatis.Actor =
    new Dramatis.Class(function Actor(behavior, name_class){
      var Cls = name_class || Actor.Name;
      this.__name__ = new Cls(this,behavior);
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
      }
    });

  (new Dramatis.Class.Subscope(Actor));
}());