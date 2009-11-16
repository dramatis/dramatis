(function(){

  var Actor = Dramatis.Actor = new Dramatis.Class(function Actor(behavior){
    return new Actor.Name(this);
  });

  new Dramatis.Class.Subscope(Actor);

})();