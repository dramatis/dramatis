(function($){

  var Server = TTT.Server = new TTT.Class( function Server() {
    this._players = [];
  }, {
    join: function(player,cb) {
      if (this.on_deck) {
        this._players.push(player);
        this._players.push(this.on_deck.player);
        var game = new TTT.Game(player,this.on_deck.player);
        cb && setTimeout(function(){
          cb(game);
        },0);
        var on_deck_cb =this.on_deck.cb;
        on_deck_cb && setTimeout(function(){
          on_deck_cb(game);
        },0);
        delete this.on_deck;
      } else {
        this.on_deck = { player: player, cb: cb };
      }
    },

    leave: function(player,cb) {
      cb && setTimeout(function(){cb();},0);
    },

    players: function(cb) {
      var v = this._players.length + ( this.on_deck ? 1 : 0 ); 
      cb && cb(v);
      return v;
    }

  });
  new TTT.Class.Subscope( Server );

})(jQuery);

