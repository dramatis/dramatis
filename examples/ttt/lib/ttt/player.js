(function($){
  var Player = TTT.Player =
    new TTT.Class(function Player(){
      this._game_callbacks = []
    }, {
      join: function(server){
        if(this.server){ throw "implement"; }
        var self = this;
        (this.server = server).join(this,function(game){
          self.current_game = game;
          if(this._game_callbacks){
            var cb;
            while(cb = this._game_callbacks.shift()){
              cb(game);
            }
            delete this._game_callbacks;
          }
        });
      },
      game: function(cb){
        if(this.current_game){
          cb(this.current_game);
        } else {
          this._game_callbacks = this._game_callbacks || [];
          this._game_callbacks.push(cb);
        }
      }
    });

})(jQuery);

