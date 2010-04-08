"use strict";
Dramatis = {
  connect: function () {
    var c = Dramatis.Director.current;
    return c.connect.apply(c, arguments);
  },
  disconnect: function (s) {
    Dramatis.Director.current.disconnect(s);
  }
};
Dramatis.toString = function () {
  return "Dramatis";
};

(function(){
  var classes = {};

  var lookup = function(string) {
    var Cls;
    if ((Cls = classes[string])) {
      return Cls;
    }
    var strings = string.split(".");
    if (!strings[0]) {
      var nil;
      return nil;
    }
    var current = (function(){return this;}());
    while(current && strings[0]) {
      current = current[ strings.shift() ];
    }
    if (current && strings.length === 0) {
      classes[string] = current;
    }
    return current;
  };

  Dramatis.revive = function revive(args) {
    if (typeof args === "object") {
      for(var key in args) {
        var v;
        if (args[key] &&
            typeof args[key] === "object" &&
            (v = args[key].__jsonclass__)) {
          args[key] = v;
        }
        revive(args[key]);
      }
    }
  };

  var reviver = function(key, value) {
    if (key === "__jsonclass__") {
      // console.debug("k",key);
      // console.debug("v",value);
      var v = value[0];
      var Cls = lookup(value.shift());
      if (Cls) {
        if (!Cls.fromJSON) {
          throw new Error(Cls+" does not have a fromJSON method");
        }
        value = Cls.fromJSON(value);
      }
    }
    return value;
  };

  Dramatis.JSON = {
    parse: function(string) {
      var result = JSON.parse(string, reviver);
      var v = result;
      if ((typeof v === "object") && (v = v.__jsonclass__)) {
        result = v;
      }
      return result;
    }
  };

  var on_error_callbacks = [];

  Dramatis.on_error = function(fn) {
    on_error_callbacks.push(fn);
  };
  
  Dramatis.error = function(string) {
    for(var i=0; i < on_error_callbacks.length; i++) {
      on_error_callbacks[i](string);
    }
  };

  var global = (function(){return this;}());

  if (global.console && global.console.debug) {
    Dramatis.on_error(function(s){
      global.console.debug(s);
    });
  }

  Dramatis.extend = function(/*...*/) {
    var object = Array.prototype.shift.apply(arguments);
    var cls = Array.prototype.shift.apply(arguments);
    for(var method in cls.prototype) {
      object[method] = cls.prototype[method];
    }
    cls.apply(object,arguments);
  };

}());
