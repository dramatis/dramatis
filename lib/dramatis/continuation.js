"use strict";
(function(){
  var Method = Dramatis.Runtime.Callable.Method;
  var window = (function(){return this;}());
  var debug = window.console && window.console.debug || window.debug;

  // actor can be either an actor name or an object with __dramatis__.name
  // methods (result, exception, timeout) can be a string or a fn

  // by functionality

  // new Continuation( result );
  // new Continuation( result, exception );
  // new Continuation( result, exception, timeout );
  // new Continuation( { result: ..., exception: ..., timeout: ... } );

  // new Continuation( actor, method )
  // new Continuation( callable )

  // by number of args

  // new Continuation( method )
  // new Continuation( function )
  // new Continuation( callable )
  // new Continuation( { result: ..., exception: ..., timeout: ... } );

  // new Continuation( actor, method )
  // new Continuation( "result", "exception" );

  // new Continuation( "result", "exception", "timeout" );

  var Continuation = Dramatis.Continuation =
    new Dramatis.Class( function Continuation(){
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
        if (typeof arguments[0] === "function") {
          // new Continuation(fn)
          throw new Error("cannot create continuation(function)");
        } else if (typeof arguments[0] === "string" || arguments[0] instanceof String) {
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