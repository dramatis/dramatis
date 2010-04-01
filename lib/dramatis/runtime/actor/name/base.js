"use strict";
(function(){
  var Runtime = Dramatis.Runtime;
  var Base = Runtime.Actor.Name.Base =
    new Dramatis.Runtime.Actor.Name.Class(function Base(){
    }, {
      send: function(method, args) {
        var future;

        if (args[args.length-1] === Dramatis.Future) {
          Array.prototype.pop(args);
          future = new Dramatis.Future();
        } else if (args[args.length-1] === Dramatis.Continue) {
          var c = Array.prototype.pop(args);
          throw new Error("implement");
        }

        var behavior;
        if (future) {
          behavior = this.actor.behavior;
          var result = behavior[method].apply(behavior,args);
          future.set(result);
          return future;
        } else {
          behavior = this.actor.behavior;
          behavior[method].apply(behavior,args);
          var undef;
          return undef;
        }
      }
    });
}());
