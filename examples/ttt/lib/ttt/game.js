(function($){
  var Game = TTT.Game =
    new TTT.Class(function Game(tom,jerry){
      this.tom = tom;
      this.jerry = jerry;
    }, {
      start: function() { this.tom.move(); }
    });
})(jQuery);

