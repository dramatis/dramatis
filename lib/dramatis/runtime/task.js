"use strict";
(function(){
  var Runtime = Dramatis.Runtime;
  var Task = Runtime.Task =
    new Dramatis.Runtime.Class(function Task(name,
                                               method,
                                               args,
                                               continuation) {
      this.name = name;
      this.method = method;
      if (!(this.args instanceof Array)) {
        this.args = Array.prototype.slice.call(args,0);
      }
      this.continuation = continuation;
    }, {
      call: function() {
        if (!(this.name instanceof Runtime.Actor.Name.Local)) {
          throw new Error("attempted to call method on remote actor");
        }
        var behavior = this.name.actor.behavior;
        var result;
        var exception;
        try {
          result = behavior[this.method].apply(behavior,this.args);
        } catch(e) {
          exception = e;
        }
        if (exception) {
          if (this.continuation) {
            this.continuation.fail(exception);
          } else {
            throw exception;
            // ? throw new Error("implement");
          }
        } else {
          if (this.continuation) {
            this.continuation.success(result);
          }
        }
      },
      toJSON: function() {
        return {
          "__jsonclass__": [
            this.constructor+"",
            this.name,
            this.method,
            this.args,
            this.continuation
          ]};
      }
    });
  Task.fromJSON = function(args) {
    return new Task(args[0],args[1],args[2],args[3]);
  };

  new Dramatis.Class.Subscope(Task);
}());
