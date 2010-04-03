"use strict";
(function(){
  var Actor = Dramatis.Actor;
  var Name = Actor.Name;

  var Type = Actor.Name.Type =
    new Actor.Name.Class(function Type(behavior) {
      var fn = function(){
        Name.apply(this,arguments);
      };
      fn.prototype = new Actor.Name();
      fn.prototype.constructor = fn;
      var closure = function(m){
        var method = m;
        fn.prototype[method] = function(){
          return this.send(method, arguments);
        };
      };
      if(behavior){
        for(var m in behavior.prototype) {
          closure(m);
        }
      }
      /* probably right or close, but not tests right now
      fn.toString = function() {
        var self = this;
        var string = behavior.__dramatis__.actor.constructor+".Name";
        fn.toString = function() {
          return string;
        };
        return fn.toString();
      };
      */
      return fn;
    });

}());