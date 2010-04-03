"use strict";
(function(){
  var Callable = Dramatis.Runtime.Callable;
  Callable.Function =
    new Dramatis.Runtime.Callable.Class(function(pair){
      Callable.apply(this,arguments);
      var fn = pair[1];
      if (typeof fn === "function") {
        this.fn_index = this.name.actor.fn_index(fn);
      } else {
        if (typeof fn !== "number") {
          throw new Error("bad fn "+fn);
        }
        this.fn_index = fn;
      }
    }, [Dramatis.Runtime.Callable ], {
      apply: function apply(args) {
        var fn = this.name.actor.fn(this.fn_index);
        var behavior = this.name.actor.behavior;
        return fn.apply(behavior, args);
      },
      toJSON: function() {
        return {
          "__jsonclass__": [
            this.constructor+"",
            this.name,
            this.fn_index
          ]};
      }
    }, {
      name: "Function"
    } );

  Callable.Function.fromJSON = function(args) {
    Dramatis.revive(args);
    return new Callable.Function(args);
  };

}());
