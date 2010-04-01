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
      if(methods){
        var closure = function(m) {
          var method = m;
          self[method] = function(){
            return self.send( method, arguments );
          };
        };
        for(var m in methods) {
          closure(m);
        }
      }
    });
  
  Name.prototype.send = function( method, args ) {
    return this.__runtime__.send(method,args);
  };

  (new Dramatis.Class.Subscope(Name));
}());