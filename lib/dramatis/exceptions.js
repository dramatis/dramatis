"use strict";
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

Dramatis.Exceptions.ServiceUnavailable =
  new Dramatis.Exceptions.Class( [ Dramatis.Exceptions.Error ], ({}), {
    name: "ServiceUnavailable"
  });

Dramatis.Exceptions.NoSuchActor =
  new Dramatis.Exceptions.Class( [ Dramatis.Exceptions.Error ], ({}), {
    name: "NoSuchActor"
  });
