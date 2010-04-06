"use strict";
(function(){
  var Callable = Dramatis.Runtime.Callable; 
  var Method = Callable.Method;
  var global = (function(){return this;}());
  var debug = global.console && global.console.debug || global.debug;

  // actor can be either an actor name or an object with __dramatis__.name
  // methods (value, exception, timeout) can be a string or a fn

  // by functionality

  // new Continuation( value );
  // new Continuation( value, exception );
  // new Continuation( value, exception, timeout );
  // new Continuation( { value: ..., exception: ..., timeout: ... } );
  // new Continuation( Continue, [ any of the above ] ) (to fake apply)

  // each of "thing", e.g., value, is a callable or something that a callable can be creted from:

  // an existing callable
  // a function
  // a method name
  // a pair, [ actor name or behavior, function or name ]

  var Continuation = Dramatis.Continuation =
    new Dramatis.Class(function Continuation(){
      var args = arguments;
      if (args[0] instanceof Dramatis.Continue) {
        args = args[1];
      }

      if(args.length === 2) {
        var first = args[0], second = args[1];

        // behavior => actor name
        if (!(first instanceof Dramatis.Actor.Name) &&
              first.__dramatis__ &&
              first.__dramatis__.name ) {
          first = first.__dramatis__.name();
        }

        if (first instanceof Dramatis.Actor.Name) {
          if (typeof second === "function") {
            // new Continuation(actor, method_name)
            throw new Error("implement new continuation(actor_name,fn)");
          } else if (second instanceof String || typeof second === "string") {
            // new Continuation(actor, fn)
            this.value = new Method( first, second );
          } else {
            throw new Error("cannont create continuation(actor_name,x)");
          }
        } else if (typeof first === "object") {
          if (typeof second === "function") {
            // new Continuation(object, fn)
            throw new Error("implement new continuation(object,fn)");
          } else if (second instanceof String || typeof second === "string") {
            // new Continuation(object, method_name)
            throw new Error("implement new continuation(object,method_name)");
            // this.value = new Method( first, second );
          } else {
            debug("io",second instanceof String);
            debug("io",typeof second);
            throw new Error("cannont create continuation(object,x)");
          }
        } else {
          throw new Error("cannont create continuation(x,y)");
        }
      } else if (args.length === 1) {
        if (typeof args[0] === "object" &&
            args[0] instanceof Array) {
          this.value = new Callable(args[0]);
        } else if (typeof args[0] === "object" ) {
          if (args[0].value) {
            this.value = new Callable(args[0].value);
          }
          if (args[0].exception) {
            this.exception = new Callable(args[0].exception);
          }
          if (args[0].timeout) {
            this.timeout = new Callable(args[0].timeout);
          }
        } else {
          if (args[0]) {
            this.value = new Callable(args[0]);
          }
        }
      } else {
        // new Continuation(fn)
        throw new Error("cannot create continuation(...)");
      }
    }, {
      toJSON: function() {
        return {
          "__jsonclass__": [
            this.constructor+"",
            this.value,
            this.exception,
            this.timeout
          ]};
      }
    });

  Continuation.fromJSON = function(args) {
    Dramatis.revive(args);
    return new Continuation({value: args[0],
                             exception: args[1],
                             timeout: args[2]});
  };

  (new Dramatis.Class.Subscope(Continuation));
}());