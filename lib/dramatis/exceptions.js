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

  Dramatis.Exceptions.Exception = 
    new Dramatis.Exceptions.Class([Dramatis.Exceptions.Error], ({
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

  (function(){
    var nil;
    var exceptions = {
      ServiceUnavailable: nil,
      NoSuchActor: nil,
      UnknownException: nil,
      MethodNotFound: nil,
      FunctionNotFound: nil
    };
    for(var exception in exceptions) {
      Dramatis.Exceptions[exception] =
        new Dramatis.Exceptions.Class( [ Dramatis.Exceptions.Exception ], ({}), {
          name: exception
        });
      Dramatis.Exceptions[exception].fromJSON = fromJSON;
    }
    
  }());

}());