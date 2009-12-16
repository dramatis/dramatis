(function(){

  var Class = TTT.Class = function(/*varargs*/){
    var constructor;
    if(arguments[0] instanceof Function){
      constructor = Array.shift.apply(arguments);
    }

    var methods = {};
    if(arguments[0] instanceof Object){
      methods = Array.shift.apply(arguments);
    }

    var options = {};
    if(arguments[0] instanceof Object){
      options = Array.shift.apply(arguments);
    }

    if(constructor === undefined){
      constructor = function(){};
    }

    var scope = options.scope;
    if(scope === undefined){
      if(this instanceof Function){
        scope = this;
      } else {
        scope = TTT;
      }
    }

    var name = options.name;
    if(name === undefined){
      name = constructor.name;
    }
    if(name === undefined){
      throw new Error("implement");
    }

    constructor.toString = (function(){
      var string = scope.toString() + "." + name;
      return function(){return string;};
    })();

    constructor.prototype = {};
    for(var method in methods){
      constructor.prototype[method] = methods[method];
    }

    return constructor;
  };

  Class.toString = (function(){
    var string = TTT.toString() + ".Class";
    return function(){return string;};
  })();

  Class.Subscope = function Subscope(fn, parent_class){
    parent_class = parent_class || Class;
    var root_subscope = Subscope;
    var name = fn.name;
    if (!name) {
      throw new Error("implement");
    }
    var cls = function Class() {
      return parent_class.apply(fn,arguments); 
    };
    cls.toString = (function(){
      var string = fn.toString() + ".Class";
      return function(){return string;};
    })();
    cls.Subscope = function Subscope(fn, parent_class) {
      parent_class = parent_class || cls;
      return root_subscope.apply(fn, parent_class);
    };
    fn.Class = cls;
  };

})();