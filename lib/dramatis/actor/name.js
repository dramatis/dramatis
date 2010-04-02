"use strict";
/*global Dramatis*/
(function(){
  var global = (function(){return this;}());
  var Name = Dramatis.Actor.Name =
    new Dramatis.Actor.Class(function Name(actor,methods){
      if (this.constructor === Name && arguments.length === 0) {
        return this;
      }
      if ((typeof arguments[0] === "string") ||
          (typeof arguments[0] === "object" && arguments[0] instanceof String)) {
        this.__runtime__ = new Dramatis.Runtime.Actor.Name.Remote(arguments);
      } else {
        this.__runtime__ = new Dramatis.Runtime.Actor.Name.Local(arguments);
      }
      var self = this;
      if(methods){
        var closure = function(m) {
          var method = m;
          self[method] = function() {
            return self.send(method, arguments);
          };
        };
        for(var m in methods) {
          // Others? Really need non-enumerable
          if (methods[m] instanceof Function &&
              m !== "toString" && 
              m !== "constructor") {
            closure(m);
          }
        }
      }
      return this;
    }, {
      toJSON: function() {
        return {
          "__jsonclass__": [
            this.constructor+"",
            this.__runtime__
          ]
        };
      }
    });
  
  Name.prototype.send = function(callable, args) {
    // console.debug("ss",$.print(args));
    return this.__runtime__.send(callable, args);
  };

  Name.extend = function(/*...*/) {
    var closure = function(n, m) {
      var name = n;
      var method = m;
      name[method] = function(){
        return name.send(method, arguments);
      };
    };
    for(var i=1; i<arguments.length; i++) {
      closure(arguments[0], arguments[1]);
    }
  };

  (new Dramatis.Class.Subscope(Name));
}());