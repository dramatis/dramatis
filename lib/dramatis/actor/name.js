"use strict";
(function(){
  var global = (function(){return this;}());
  var Name = Dramatis.Actor.Name =
    new Dramatis.Actor.Class(function Name(actor,methods){
      if (this.constructor === Name && arguments.length === 0) {
        return this;
      }
      if (arguments[0] instanceof Dramatis.Runtime.Actor.Name.Base) {
        this.__runtime__ = arguments[0];
      } else if (arguments[0] instanceof Dramatis.Actor.Name) {
        // CHECK: mutability?
        this.__runtime__ = arguments[0].__runtime__;
      } else if ((typeof arguments[0] === "string") ||
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
      // Huh?/ugh
      add_subscription: function(/*...*/) {
        return this.send("add_subscription", arguments);
      },
      remove_subscription: function(/*...*/) {
        return this.send("remove_subscription", arguments);
      },
      toJSON: function() {
        // console.debug("tj",this.__runtime__.constructor+"");
        // JSON.stringify(this.__runtime__);
        // console.debug("11");
        return {
          "__jsonclass__": [
            this.constructor+"",
            this.__runtime__
          ]
        };
      },
      equals: function(that) {
        //] console.debug(">",$.print(this));
        // console.debug("<",$.print(that));
        var self = this.__dramatis__ && this.__dramatis__.actor.__name__ || this;
        that = that.__dramatis__ && that.__dramatis__.actor.__name__ || that;
        var self_id = self.__runtime__.id;
        var that_id = that.__runtime__.id;
        var self_type = typeof self_id;
        var that_type = typeof that_id;
        if (self_type !== that_type) {
          throw new Error("comparisson of actor names of different types not suported");
        }
        return self_id === that_id;
      }
    });
  
  Name.route = function(name) {
    return name.__runtime__.route;
  };

  Name.id = function(name) {
    return name.__runtime__ && name.__runtime__.id;
  };

  Name.behavior = function(name) {
    return name.__runtime__ && name.__runtime__.actor() && name.__runtime__.actor().behavior;
  };

  Name.fromJSON = function(args) {
    Dramatis.revive(args);
    var Cls = this;
    return new Cls(args[0]);
  };

  Name.uri = function(name) {
    return name.__runtime__.uri();
  };

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