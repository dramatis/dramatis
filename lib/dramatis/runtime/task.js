"use strict";
(function(){
  var global = (function(){return this;}());
  var Runtime = Dramatis.Runtime;
  var Task = Runtime.Task =
    new Dramatis.Runtime.Class(function Task(callable, args, continuation) {
      this.callable = callable;
      this.args = args;
      if (this.args && !(this.args instanceof Array)) {
        // console.debug("task",$.print(args),typeof args);
        if (typeof args !== "object") {
          throw new Error("argument error: "+typeof args);
        }
        this.args = Array.prototype.slice.call(args,0);
      }
      this.continuation = continuation;
    }, {
      call: function() {
        var value;
        var exception;
        try {
          value = this.callable.apply(this.args);
        } catch(e) {
          exception = e;
        }
        // console.debug("H!",value,exception,this.continuation);
        if (exception) {
          if (this.continuation) {
            this.continuation.exception.raise(exception);
          } else {
            // FIX: !!!!!!!!!! throw new Error("implement"); terminate?
            // console.debug("uncaught exception: "+exception);
            global.console.debug("uncaught exception: "+exception.stack);
          }
        } else {
          if (this.continuation) {
            this.continuation.value.call(value);
          }
        }
      },
      toJSON: function() {
        return {
          "__jsonclass__": [
            this.constructor+"",
            this.callable,
            this.args,
            this.continuation
          ]};
      }
    });
  Task.fromJSON = function(args) {
    Dramatis.revive(args);
    // console.debug("tfj",$.print(args));
    return new Task(args[0],args[1],args[2]);
  };

  new Dramatis.Class.Subscope(Task);
}());
