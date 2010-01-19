(function($){
  var Actor = Dramatis.Actor;
  var Name = Actor.Name;

  var Behavior = Actor.Behavior =
    new Actor.Class(function Behavior(args) {
      this.__dramatis__ = Array.prototype.shift.call(args);
      this.__dramatis__.become(this);
    });

})(jQuery);