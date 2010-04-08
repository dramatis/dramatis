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
      remove_continuation: function() {
        var continuation = this.continuation;
        var nil;
        this.continuation = nil;
        return continuation;
      },
      call: function() {
        var value;
        var exception;
        try {
          value = this.callable.apply(this.args);
        } catch(e) {
          exception = e;
        }

        // Not sure about the semantics here: it's expected that
        // raise will raise a new exception ... ? Intestingly, gives
        // the actor the chance to pass the original excpetion on ...
        // ... for better or worse
        if (exception) {
          try {
            this.callable.name.actor().raise(exception);
          } catch(e1) {
            exception = e1;
          }
        }

        // console.debug("H!",value,exception,this.continuation);
        if (exception) {
          if (this.continuation && this.continuation.exception) {
            this.continuation.exception.raise(exception);
          } else {
            Dramatis.error("uncaught exception: "+exception+
                           " ("+exception.constructor+") "+exception.stack);
          }
        } else {
          if (this.continuation && this.continuation.value) {
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
