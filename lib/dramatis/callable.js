"use strict";
(function(){
  var Method = Dramatis.Runtime.Callable.Method;
  var window = (function(){return this;}());
  var debug = window.console && window.console.debug || window.debug;

  var Continuation = Dramatis.Continuation =
    new Dramatis.Class( function Continuation(){
      // FIX? Shouldn't be changing arugments?
      var args = arguments;
      if(args.length === 2) {

        // behavior => actor name
        if (!(args[0] instanceof Dramatis.Actor.Name) &&
              args[0].__dramatis__ &&
              args[0].__dramatis__.name) {
          args[0] = args[0].__dramatis__.name();
        }

        if (args[0] instanceof Dramatis.Actor.Name) {
          if (typeof args[1] === "function") {
            // new Continuation(actor, method_name)
            throw new Error("implement new continuation(actor_name,fn)");
          } else if (args[1] instanceof String || typeof args[1] === "string") {
            // new Continuation(actor, fn)
            throw new Error("implement new continuation(actor_name,fn)");
          } else {
            throw new Error("cannont create continuation(actor_name,x)");
          }
        } else if (typeof args[0] === "object") {
          if (typeof args[1] === "function") {
            // new Continuation(object, fn)
            throw new Error("implement new continuation(object,fn)");
          } else if (args[1] instanceof String || typeof args[1] === "string") {
            // new Continuation(object, method_name)
            throw new Error("implement new continuation(object,method_name)");
            // this.value = new Method( args[0], args[1] );
          } else {
            debug("io",args[1] instanceof String);
            debug("io",typeof args[1]);
            throw new Error("cannont create continuation(object,x)");
          }
        } else {
          throw new Error("cannont create continuation(x,y)");
        }
      } else if (args.length === 1) {
        if (typeof args[0] === "function") {
          // new Continuation(fn)
          throw new Error("cannot create continuation(function)");
        } else if (typeof args[0] === "string" || args[0] instanceof String) {
          // new Continuation(method_name)
          throw new Error("cannot create continuation(method_name)");
        } else {
          throw new Error("cannot create continuation(x)");
        }
      } else {
        // new Continuation(fn)
        throw new Error("cannot create continuation(...)");
      }
    }, {
      call: function call() {
        this.value.apply( arguments );
      }
    });

  (new Dramatis.Class.Subscope(Continuation));
}());