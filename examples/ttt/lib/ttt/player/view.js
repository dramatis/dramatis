(function($){

  var Player = TTT.Player;
  var View = Player.View = function(player,dom){
    this.player = player;
    this.bind( "notification" );
  };

  View.toString = (function(){
    var string = Player.toString() + ".View";
    return function(){return string;};
  })();

  View.prototype.bind = function(/* varargs */) {
    var self = this;
    for(var i=0; i < arguments.length; i++) {
      var name = arguments[i];
      var fn = this[name];
      this[name] = function( /* arargs */ ) { fn.apply(self,arguments); };
    }
  };

  View.prototype.notification = function() {
    throw "implement";
  };

})(jQuery);

