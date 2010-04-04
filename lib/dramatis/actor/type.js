"use strict";
/*jslint evil:true*/
(function(){
  var Actor = Dramatis.Actor;

  var ctor_apply = function ctor_apply(fn,a) {
    var args = [];
    for(var i = 0; i < a.length; i++ ) {
      args.push( "a[" + i + "]" );
    }
    var string = "new fn(" + args.join(",") + ")";
    return eval(string);
  };

  var toJSON = function() {
    return this.__dramatis__.actor.__name__.toJSON();
  };

  var Type = Actor.Type =
    new Actor.Class(function Type(fn,methods) {
      var cls = function(){
        var nil;
        var actor = new Actor(nil, cls.Name);
        var actor_interface = new Actor.Interface(actor.__runtime__.actor());
        Array.prototype.unshift.call(arguments,actor_interface);
        var behavior = ctor_apply(fn, arguments);
        actor_interface.become(behavior);
        if (!behavior.toJSON) {
          behavior.toJSON = toJSON;
        }
        return actor;
      };

      for(var v in methods) {
        fn.prototype[v] = methods[v];
      }
      
      cls.Name = new Actor.Name.Type(fn);

      return cls;
    });

}());