"use strict";
(function(){
  var Actor = Dramatis.Actor;
  var Name = Actor.Name;

  var Type = Actor.Name.Type =
    new Actor.Name.Class(function Type(object, methods) {
      // needs test for non-behavior path
      var constructor, behavior, base_name;
      if (object instanceof Function) {
        constructor = object;
        base_name = object.toString();
      } else if (object instanceof Array) {
        methods = object;
      } else if (typeof object === "string" ||
                 object instanceof String) {
        base_name = object.toString();
      } else {
        behavior = object;
        if (behavior) {
          // ??
          if (behavior.__dramatis__ &&
              behavior.__dramatis__.actor &&
              behavior.__dramatis__.actor.constructor) {
            base_name =behavior.__dramatis__.actor.constructor+""; 
          } else {
            base_name = behavior.constructor+"";
          }
        }
      }

      var fn = function() {
        Name.apply(this,arguments);
      };
      fn.prototype = new Actor.Name();
      fn.prototype.constructor = fn;

      fn.fromJSON = Name.fromJSON;

      var closure = function(m){
        var method = m;
        if (method !== "toString" &&
            method !== "constructor") {
          fn.prototype[method] = function(){
            return this.send(method, arguments);
          };
        }
      };

      var m;

      if (constructor) {
        for(m in constructor.prototype) {
          closure(m);
        }
      }
      
      if (behavior) {
        for(m in behavior.prototype) {
          closure(m);
        }
      }

      if (methods) {
        for(m=0; m<methods.length; m++) {
          closure(methods[m]);
        }
      }

      if (base_name) {
        fn.toString = function() {
          var self = this;
          var string = base_name+".Name";
          fn.toString = function() {
            return string;
          };
          return fn.toString();
        };
      }

      return fn;
    });
 
}());