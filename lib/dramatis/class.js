"use strict";

(function () {

  /*global Dramatis*/

  var Class = Dramatis.Class = function (/*...*/) {
    var constructor,
        methods = {},
        options = {},
        scope,
        name,
        string,
        method;


    if (arguments[0] instanceof Function) {
      constructor = Array.shift.apply(arguments);
    }

    if (arguments[0] instanceof Object) {
      methods = Array.shift.apply(arguments);
    }

    if (arguments[0] instanceof Object) {
      options = Array.shift.apply(arguments);
    }

    if (constructor === undefined) {
      constructor = function () {};
    }

    scope = options.scope;
    if (scope === undefined) {
      if (this instanceof Function) {
        scope = this;
      } else {
        scope = Dramatis;
      }
    }

    name = options.name;

    if (name === undefined) {
      name = constructor.name;
    }
    if (name === undefined) {
      throw new Error("implement");
    }

    constructor.toString = (function () {
      string = scope.toString() + "." + name;
      return function () {
        return string;
      };
    }());

    constructor.prototype = {};
    for (method in methods) {
      constructor.prototype[method] = methods[method];
    }

    return constructor;
  };

  Class.toString = (function () {
    var string = Dramatis.toString() + ".Class";
    return function () {
      return string;
    };
  }());

  Class.Subscope = function Subscope(fn, parent_class) {
    parent_class = parent_class || Class;
    var root_subscope = Subscope,
        name = fn.name,
        cls,
        string;
    if (!name) {
      throw new Error("implement");
    }
    cls = function Class() {
      return parent_class.apply(fn, arguments); 
    };
    cls.toString = (function () {
      string = fn.toString() + ".Class";
      return function () {
        return string;
      };
    }());
    cls.Subscope = function Subscope(fn, parent_class) {
      parent_class = parent_class || cls;
      return root_subscope.apply(fn, parent_class);
    };
    fn.Class = cls;
  };

}());