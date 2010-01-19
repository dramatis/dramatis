(function($){
  var Interface = Dramatis.Actor.Interface =
    new Dramatis.Actor.Class(function Interface(actor){
      this.actor = actor;
    }, {
      become: function become(behavior) {
        this.actor.become(behavior);
      },
      name: function name(fn) {
        return this.actor.name(fn);
      }
    });
})(jQuery);