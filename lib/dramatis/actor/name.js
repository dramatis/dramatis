"use strict";
/*global Dramatis*/
(function(){
  var Name = Dramatis.Actor.Name =
    new Dramatis.Actor.Class(function Name(actor,methods){
      var self = this;
      this.__dramatis__ = {};
      this.__dramatis__.actor = actor;
      if(methods){
        for(var m in methods) {
          (function(){
            var method = m;
            self[method] = function(){
              return self.send( method, arguments );
            };
          }());
        }
      }
    });
  
  Name.prototype.send = function( method, args ) {
    var behavior;
    if(args.length &&
       args[args.length-1] === Dramatis.Future){
      var future = new Dramatis.Future();
      behavior = this.__dramatis__.actor.behavior;
      var result = behavior[method].apply(behavior,args);
      future.set(result);
      return future;
    } else {
      behavior = this.__dramatis__.actor.behavior;
      behavior[method].apply(behavior,args);
      var undef;
      return undef;
    }
  };

  (new Dramatis.Class.Subscope(Name));
}());