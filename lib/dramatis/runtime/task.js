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
      this.args = args;
      this.continuation = continuation;
    }, {
      call: function() {
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
      }
    });
  new Dramatis.Class.Subscope(Task);
}());
