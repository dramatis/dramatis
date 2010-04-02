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

  var reviver = function(key, value) {
    if (key === "__jsonclass__") {
      var v = value[0];
      var Cls = lookup(value.shift());
      if (Cls) {
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
}());
