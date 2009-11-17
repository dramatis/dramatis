(function(){
  var Actor = Dramatis.Actor;
  var Name = Actor.Name;
  var Type = Actor.Type =
    new Actor.Class(function Type(fn,methods) {
      var cls = function(){
        return new Name(this);
      };
      for(var v in methods) {
        cls.prototype[v] = methods[v];
      }
      return cls;
    });
})();