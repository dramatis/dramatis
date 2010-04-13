"use strict";
(function(){

  Dramatis.Exceptions = new Dramatis.Class(function Exceptions(){});
  (new Dramatis.Class.Subscope(Dramatis.Exceptions));

  Dramatis.Exceptions.Error = 
    new Dramatis.Exceptions.Class(function() {
          var error = Error.prototype.constructor.apply(this,arguments);
      for(var key in error) {
        if (error.hasOwnProperty(key)) {
          this[key] = error[key];
        }
      }
    }, ({}), {
      name: "Error"
    });

  Dramatis.Exceptions.Error.prototype = Error.prototype;

  Dramatis.Exception = 
    new Dramatis.Class([Dramatis.Exceptions.Error], ({
      toJSON: function() {
        return {
          "__jsonclass__": [
            this.constructor+"",
            this.message,
            this.stack
          ] };
      }
    }), {
      name: "Exception"
    });

  var fromJSON = function(args) {
    Dramatis.revive(args);
    var Cls = this;
    var exception = new Cls(args[0]);
    exception.stack = args[1];
    return exception;
  };

  (new Dramatis.Class.Subscope(Dramatis.Exception));

  (function(){
    var nil;
    var exceptions = {
      Raise: nil,
      ServiceUnavailable: nil,
      NoSuchActor: nil,
      UnknownException: nil,
      MethodNotFound: nil,
      FunctionNotFound: nil,
      Terminated: {
        Normal: nil,
        Other: nil
      }
    };

    var builder = function(parent, hash) {
      for(var exception in hash) {
        parent[exception] =
          new parent.Class( [ parent ], ({}), {
            name: exception
          });
        // console.debug(parent[exception]+"");
        parent[exception].fromJSON = fromJSON;
        if (hash[exception]) {
          new Dramatis.Class.Subscope(parent[exception]);
          builder(parent[exception],hash[exception]);
        }
      }
    };
    
    builder(Dramatis.Exception, exceptions);

  }());

}());