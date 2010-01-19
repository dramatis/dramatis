(function($){
  var Name = Dramatis.Actor.Name =
    new Dramatis.Actor.Class(function Name(actor,methods){
      var self = this;
      this.__dramatis__ = {};
      this.__dramatis__.actor = actor;
      if(methods){
        for(var m in methods) {
          (function(){
            var method = m;
            self[method] = function(){
              self.send( method, arguments );
            };
          })();
        }
      }
    });
  
  Name.prototype.send = function( method, arguments ) {
    var behavior = this.__dramatis__.actor.behavior;
    behavior[method].apply(behavior,arguments);
  };

  new Dramatis.Class.Subscope(Name);
})(jQuery);