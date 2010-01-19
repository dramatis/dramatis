(function($){
  var Actor = Dramatis.Actor;
  var Name = Actor.Name;

  var ctor_apply = function ctor_apply(fn,a) {
    var args = [];
    for(var i = 0; i < a.length; i++ ) {
      args.push( "a[" + i + "]" );
    }
    var string = "new fn(" + args.join(",") + ")";
    return eval(string);
  };


  var Type = Actor.Type =
    new Actor.Class(function Type(fn,methods) {
      var cls = function(){
        var undef;
        var actor = new Actor(undef, cls.Name);
        var actor_interface = new Actor.Interface(actor.__dramatis__.actor);
        Array.prototype.unshift.call(arguments,actor_interface);
        var behavior = ctor_apply(fn, arguments);
        actor_interface.become(behavior);
        return actor;
      };

      for(var v in methods) {
        fn.prototype[v] = methods[v];
      }
      
      cls.Name = new Actor.Name.Type(fn);

      return cls;
    });

})(jQuery);