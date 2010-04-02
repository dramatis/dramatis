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

  // each of "thing", e.g., value, is a callable or something that a callable can be creted from:

  // an existing callable
  // a function
  // a method name
  // a pair, [ actor name or behavior, function or name ]

  var Continue = Dramatis.Continue =
    new Dramatis.Class(function Continue(/*...*/){
      if (this === global) {
        return new Continue(arguments);
      }
      return this; // keep emacs happy
    });

  var Continuation = Dramatis.Continuation =
    new Dramatis.Class(function Continuation(){
      if(arguments.length === 2) {
        var first = arguments[0], second = arguments[1];

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
      } else if (arguments.length === 1) {
        if (typeof arguments[0] === "object" &&
            arguments[0] instanceof Array) {
          this.value = new Callable(arguments[0]);
        } else if (typeof arguments[0] === "object" ) {
          this.value = new Callable(arguments[0].value);
          this.exception = new Callable(arguments[0].exception);
          this.timeout = new Callable(arguments[0].timeout);
        } else {
          throw new Error("cannot create continuation from "+arguments[0]);
        }
      } else {
        // new Continuation(fn)
        throw new Error("cannot create continuation(...)");
      }
    });

  (new Dramatis.Class.Subscope(Continuation));
}());