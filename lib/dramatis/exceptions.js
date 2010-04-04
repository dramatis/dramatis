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

  var toJSON = function() {
    return {
      "__jsonclass__": [
        this.constructor+"",
        this.message,
        this.stack
      ] };
  };

  var fromJSON = function(args) {
    Dramatis.revive(args);
    var Cls = this;
    var exception = new Cls(args[0]);
    exception.stack = args[1];
    return exception;
  };

  Dramatis.Exceptions.ServiceUnavailable =
    new Dramatis.Exceptions.Class( [ Dramatis.Exceptions.Error ], ({
      toJSON: toJSON
    }), {
      name: "ServiceUnavailable"
    });
  Dramatis.Exceptions.ServiceUnavailable.fromJSON = fromJSON;

  Dramatis.Exceptions.NoSuchActor =
    new Dramatis.Exceptions.Class( [ Dramatis.Exceptions.Error ], ({
      toJSON: toJSON
    }), {
      name: "NoSuchActor"
    });

  Dramatis.Exceptions.NoSuchActor.fromJSON = fromJSON;

}());