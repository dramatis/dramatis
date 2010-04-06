"use strict";
(function(){
  var global = (function(){return this;}());
  if (!global.console) {
    global.console = {};
    global.console.debug = global.debug;
  }
}());
jazz.include("lib/dramatis.js");
