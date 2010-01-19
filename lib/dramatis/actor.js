(function(){

  var Actor = Dramatis.Actor =
    new Dramatis.Class(function Actor(behavior, name_class){
      this.__name__ = new (name_class || Actor.Name)(this,behavior);
      this.become(behavior);
      return this.__name__;
    }, {
      become: function become(behavior) {
        // FIX: detach old behavior
        this.behavior = behavior;
        if(behavior){
          behavior.__dramatis__ =
            behavior.__dramatis__ || new Actor.Interface(this);
        }
      },
      name: function name(fn) {
        fn && fn(this.__name__);
        return this.__name__;
      }
    });

  new Dramatis.Class.Subscope(Actor);

})();