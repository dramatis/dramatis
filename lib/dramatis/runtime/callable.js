"use strict";
(function(){
  var global = (function(){return this;}());

  var Runtime = Dramatis.Runtime;

  // ceateable from
  // an existing callable
  // a function
  // a method name
  // a pair, [ actor referenc, function or name ]

  // in the case of a pair, it can be called either with an explict array
  // or simply two arguments
 
  var Callable = Runtime.Callable =
    new Dramatis.Runtime.Class(function Callable(/*...*/){
      /* prototype constructor */
      if (this.constructor === Callable && arguments.length === 0) {
        return this;
      }

      /* derived constructor */
      if (this.constructor !== Callable) {
        this.name = arguments[0][0];
        /*
        if (this.name instanceof Dramatis.Actor.Name) {
          this.name = this.name.__runtime__;
        }
        */
        if (!(this.name instanceof Runtime.Actor.Name.Base)) {
          throw new Error("wrong type for callable: "+this.name.constructor+" "+this.constructor);
        }
        return this;
      }

      var pair;
      if (arguments.length === 1) {
        var nil;
        if (arguments[0] === nil || arguments[0] === null) {
          try { 
            throw new Error("cannot create callable from undefined or null"); 
          } catch(e) {
            global.console.debug(e.stack);
            throw e;
          }
        }

        if (arguments[0] instanceof Callable) {
          // FIX if not read only ...
          return arguments[0];
        } else if (arguments[0] instanceof Array) {
          if (arguments[0].length === 2) {
            pair = arguments[0];
          }
        } else {
          pair = [ Dramatis.Director.current.current_actor(), arguments[0] ];
        }            
      } else if (arguments.length === 2) {
        pair = arguments;
      }

      if (pair[1] instanceof Callable) {
        if (!(pair[0].equals(pair[1].name))) {
          // new Error("callable targeted to wrong name: " + $.print(pair[0]) + " vs " + $.print(pair[1].name));
          new Error("callable targeted to wrong name");
        }
        return pair[1];
      }

      if (!pair) {
        throw new Error("could not create callable");
        //throw new Error("could not create callable: "+$.print(arguments));
      }

      if (pair[0] instanceof Dramatis.Actor.Name) {
        pair[0] = pair[0].__runtime__;
      }

      if (!(pair[0] instanceof Dramatis.Runtime.Actor.Name.Base)) {
        throw new Error("cannot create actor name from "+
                         pair[0]+ " " +pair[0].constructor+"");
      }

      if (typeof pair[1] === "function" ) {
        var fn = new Callable.Function(pair);
        return fn;
      }
      if (typeof pair[1] === "string" ||
          typeof pair[1] === "object" && pair[1] instanceof String) {
        var m = new Callable.Method(pair);
        return m;
      }
      throw new Error("could not create callable from "+pair[1]);
    }, {
      call: function call(/*...*/) {
        // console.debug("call",$.print(arguments));
        this.name.send(this,arguments);
      },
      raise: function raise(exception) {
        // console.debug("raise",$.print(exception));
        if (!(exception instanceof Dramatis.Exceptions.Exception)) {
          exception = new Dramatis.Exceptions.UnknownException(exception);
          // console.debug("raisex",$.print(exception));
        }
        // console.debug("raise",exception.constructor+""+exception);
        this.name.send(this,[exception]);
      },
      toJSON: function() {
        if (this.constructor !== Callable) {
          throw new Error("implement for "+this.constructor+"");
        }
        return null;
      }
    });
  new Dramatis.Class.Subscope(Callable);
}());
