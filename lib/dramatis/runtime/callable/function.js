"use strict";
(function(){
  var Callable = Dramatis.Runtime.Callable;
  Callable.Function =
    new Dramatis.Runtime.Callable.Class(function(pair){
      Callable.apply(this,arguments);
      var fn = pair[1];
      if (typeof fn === "function") {
        this.fn_index = this.name.actor().fn_index(fn);
      } else {
        if (typeof fn !== "number") {
          throw new Error("bad fn "+fn);
        }
        this.fn_index = fn;
      }
    }, [Dramatis.Runtime.Callable ], {
      fn: function(target) {
        var fn = this.name.actor().fn(this.fn_index);
        if (!fn) {
          throw new Dramatis.Exception.
            FunctionNotFound("Function not found for index '"+this.fn_index+
                             "' for actor id "+this.name.id);
        }
        return fn;
      },
      toJSON: function() {
        return {
          "__jsonclass__": [
            this.constructor+"",
            this.name,
            this.fn_index,
            this.type
          ]};
      }
    }, {
      name: "Function"
    } );

  Callable.Function.fromJSON = Callable.fromJSON;

}());
