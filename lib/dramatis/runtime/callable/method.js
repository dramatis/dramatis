"use strict";
(function(){
  var Callable = Dramatis.Runtime.Callable;
  Callable.Method =
    new Dramatis.Runtime.Callable.Class(function Method(pair) {
      Callable.apply(this,arguments);
      if (!pair[0] || !pair[1]) {
        throw new Error("bad args");
      }
      this.method = pair[1];
    }, [Callable ], {
      apply: function apply(args) {
        var behavior = this.name.actor().behavior;
        var method = behavior[this.method];
        if (!method) {
          throw new Dramatis.Exceptions.
            MethodNotFound("Method '"+this.method+"' not found for actor id "+this.name.id);
        }
        return method.apply(behavior, args);
      },
      toJSON: function() {
        return {
          "__jsonclass__": [
            this.constructor+"",
            this.name,
            this.method
          ]};
      }
    });
  Callable.Method.fromJSON = function(args) {
    Dramatis.revive(args);
    return new Callable.Method(args);
  };

}());
