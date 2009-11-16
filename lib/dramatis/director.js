(function(){

  var Actor = Dramatis.Actor;
  var Name = Actor.Name;
  var Director = Dramatis.Director =
    new Dramatis.Actor.Type(function Director(){
    });

  var current;

  Director.__defineGetter__("current", function(){
    current = current || new Director;
    return current;
  });

})();