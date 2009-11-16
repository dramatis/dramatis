(function(){
  var Actor = Dramatis.Actor;
  var Name = Actor.Name;
  var Type = Actor.Type =
    new Actor.Class(function Type(fn) {
      var cls = function(){
        return new Name(this);
      };
      return cls;
    });
})();