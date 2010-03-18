"use strict";
(function(){

  var include = function(fn) {
    // 1: during parse
    // -> use document.write
    // 2: outside of parse
    //  a: by jazz
    //  -> use jazz
    //  b: not by jazz
    //  -> use?

    // for now: use jazz if it exists, else doc write
    // not perfect match, but good enough for now?

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
        fn(prefix.concat(object).join("/"));
      } else if(object instanceof Array) {
        for(var i=0; i < object.length; i++) {
          each(prefix,object[i],fn);
        }
      } else {
        for(var key in object) {
          each(prefix.concat(key),object[key],fn);
        }
      }
    }([],object,fn));
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
          "callable",
          {"callable": "method"},
          "reactor",
          { "reactor": [
            "channel",
            { "channel": [
              "xmpp"
            ] }
          ] }
        ] },
        "future",
        "continuation",
        "subscriber",
        "publisher"
      ] }
    };

  each(javascript,function(fn) {
    include(fn+".js");
  });

  /*
  var css = { "lib": { "testflock": [ "testflock", "index", "show", "captured" ] } };
  each(css, function(fn) {
    throw new Error("hell");
    document.write("<link href='"+fn+".css' rel='stylesheet' type='text/css'>");
  });
  */

}());

