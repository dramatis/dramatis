"use strict";
(function () {

  /*global Dramatis*/

  var Class = Dramatis.Class = function (/*...*/) {

    var constructor;
    if (arguments[0] instanceof Function) {
      constructor = Array.shift.apply(arguments);
    }

/*
    var Base;
    if(arguments[0] instanceof Function){
      Base = Array.shift.apply(arguments);
    }
*/

    var mixins;
    if(arguments[0] instanceof Array){
      mixins = Array.shift.apply(arguments);
    }

    var methods = {};
    if (arguments[0] instanceof Object) {
      methods = Array.shift.apply(arguments);
    }

    var options = {};
    if (arguments[0] instanceof Object) {
      options = Array.shift.apply(arguments);
    }

    if (constructor === undefined) {
      constructor = function () {};
    }

    var scope = options.scope;
    if (scope === undefined) {
      if (this instanceof Function) {
        scope = this;
      } else {
        scope = Dramatis;
      }
    }

    var named_scope = true;

    var name = options.name;
    if (name) {
      named_scope = false;
    }

    if (name === undefined) {
      name = constructor.name;
    }

    if (!(name === undefined || name === "")) {
      constructor.toString = (function () {
        var string = ( named_scope ? scope.toString() + "." : "" ) + name;
        return function () {
          return string;
        };
      }());
    }

    constructor.prototype = {};

    var method;
    if (mixins) {
      for(var i in mixins){
        var mixin = mixins[i].prototype;
        for(method in mixin){
          constructor.prototype[method] = mixin[method];
        }
      }
    }

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