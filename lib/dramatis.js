"use strict";
var Dramatis = {
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