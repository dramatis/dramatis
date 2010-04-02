"use strict";
/*global Dramatis*/
(function(){
  var Name = Dramatis.Actor.Name =
    new Dramatis.Actor.Class(function Name(actor,methods){
      if ((typeof arguments[0] === "string") ||
          (typeof arguments[0] === "object" && arguments[0] instanceof String)) {
        this.__runtime__ = new Dramatis.Runtime.Actor.Name.Remote(arguments);
      } else {
        this.__runtime__ = new Dramatis.Runtime.Actor.Name.Local(arguments);
      }
      var self = this;
      console.debug("m",actor,$.print(methods));
      if(methods){
        var closure = function(m) {
          var method = m;
          console.debug("!!",m);
          self[method] = function() {
            return self.send( method, arguments );
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
  
  Name.prototype.send = function( method, args ) {
    return this.__runtime__.send(method,args);
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