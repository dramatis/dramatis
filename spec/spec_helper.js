"use strict";
jazz.include("lib/dramatis.js");
(function(){
  var global = (function(){return this;}());
  if (!global.console) {
    global.console = {};
    global.console.debug = debug;
  }
}());
