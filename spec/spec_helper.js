"use strict";
(function(){
  var global = (function(){return this;}());
  if (!global.console) {
    global.console = {};
    global.console.debug = global.debug;
  }

  if (!global.backtrace) {
    global.backtrace = function() {
      try {
        throw new Error("backtrace");
      } catch(e) {
        global.console.debug(e.stack);
      }
    };
  }
}());
jazz.include("lib/dramatis.js");
