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
      fn: function apply(target) {
        var method = target[this.method];
        if (!method) {
          throw new Dramatis.Exception.
            MethodNotFound("Method '"+this.method+
                           "' not found for actor id "+this.name.id+
                          " type '"+this.type+"'");
        }
        return method;
      },
      toJSON: function() {
        return {
          "__jsonclass__": [
            this.constructor+"",
            this.name,
            this.method,
            this.type
          ]};
      }
    });

  Callable.Method.fromJSON = Callable.fromJSON;

}());
