"use strict";
(function(){

  var include = function(fn,prefix) {
    var global = (function(){return this;}());
    if (global.jazz) {
      global.jazz.include(fn);
    } else {
      global.document.write("<script src='"+fn+"'></script>");
    }
  };

  var each = function(object, fn) {
    return (function each(prefix, object, fn) {
      if(typeof object === "string" ||
         typeof object === "object" && object instanceof String) {
        fn(prefix.concat(object).join("/"),prefix);
      } else if(object instanceof Array) {
        var first = 0;
        if (object[0] instanceof Function) {
          if (!object[0]()) {
            return;
          }
          first = 1;
        }
        for(var i=first; i < object.length; i++) {
          each(prefix,object[i],fn);
        }
      } else {
        for(var key in object) {
          each(prefix.concat(key),object[key],fn);
        }
      }
    }([], object, fn));
  };

  var javascript =
    { "vendor": {
      "jquery/dist": [ "jquery" ],
      "jquery.print/dist": [ "jquery.print" ], // FIX
      "strophejs": {
        "src" : [ "base64", "md5", "core" ],
        "contrib": "mock/strophe" },
      "feste": [ "lib/feste/feste", "lib/feste/class" ],
      "puck": [ "lib/puck/puck", "lib/puck/publisher", "lib/puck/subscriber" ]
    },
      "lib":
      { "dramatis": [
        "dramatis",
        "class",
        "exceptions",
        "publisher",
        "actor",
        {"actor": "type"},
        {"actor":
         [ "behavior",
           "name",
           { "name": "type" },
           "interface"
         ] },
        "director",
        "runtime",
        { "runtime": [
          "task",
          "actor",
          {"actor": [
            "name",
            {"name": [ "base", "local", "remote" ] } ] },
          "callable",
          {"callable": ["method", "function"]},
          "reactor",
          { "reactor": [
            "channel",
            { "channel": [
              "xmpp"
            ] }
          ] }
        ] },
        "future",
        "continue",
        "continuation",
        "subscriber"
      ] }
    };

  each(javascript,function(fn,prefix) {
    include(fn+".js",prefix);
  });

  /*
  var css = { "lib": { "dramatis": [ "dramatis", "index", "show", "captured" ] } };
  each(css, function(fn) {
    throw new Error("hell");
    document.write("<link href='"+fn+".css' rel='stylesheet' type='text/css'>");
  });
  */

}());

