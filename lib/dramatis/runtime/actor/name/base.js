"use strict";
(function(){
  var Runtime = Dramatis.Runtime;
  var Base = Runtime.Actor.Name.Base =
    new Dramatis.Runtime.Actor.Name.Class(function Base(){
    }, {
      send: function(method, args) {
        var future;
        var continuation;
        if (args[args.length-1] === Dramatis.Future) {
          Array.prototype.pop(args);
          continuation = future = new Dramatis.Future();
        } else if (args[args.length-1] === Dramatis.Continue) {
          var c = Array.prototype.pop(args);
          throw new Error("implement");
        }
        Dramatis.Director.current.
          enqueue(new Dramatis.Runtime.Task(this, method, args, continuation));
        return future;
      }
    });
}());
