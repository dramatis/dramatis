"use strict";
(function(){
  var Runtime = Dramatis.Runtime;
  var Base = Runtime.Actor.Name.Base =
    new Dramatis.Runtime.Actor.Name.Class(function Base(){
    }, {
      send: function(callable, args) {
        var future;
        var continuation;
        // console.debug("v", args[args.length-1] ? $.print(args[args.length-1].constructor+"") : "falsey");
        if (args[args.length-1] === Dramatis.Future) {
          Array.prototype.pop(args);
          continuation = future = new Dramatis.Future();
        } else if (args[args.length-1] instanceof Dramatis.Continue) {
          continuation = Array.prototype.pop.apply(args).continuation;
        }
        Dramatis.Director.current.
          enqueue(new Dramatis.Runtime.Task(new Runtime.Callable([this, callable]), args, continuation));
        return future;
      },
      equals: function(that) {
        if (typeof this.id !== "number" ||
            typeof that.id !== "number") {
          // console.debug(this.constructor+"",$.print(this));
          // console.debug(that.constructor+"",$.print(that));
          throw new Error("implement "+typeof this.id+" "+typeof that.id);
        }
        return this.id === that.id;
      }
    });
}());
