(function(){
  var Actor = Dramatis.Actor;
  var Name = Actor.Name;

  var Type = Actor.Name.Type =
    new Actor.Name.Class(function Type(behavior) {
      var fn = function(){
        Name.apply(this,arguments);
      };
      fn.prototype = new Actor.Name;
      fn.prototype.constructor = fn;
      if(behavior){
        for(m in behavior.prototype) {
          (function(){
            var method = m;
            fn.prototype[method] = function(){
              this.send( method, arguments );
            };
          })();
        }
      }
      return fn;
    });

})();