jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/ttt/spec_helper.js");

(function(){

  describe("ttt",function(){
    describe("server",function(){

      it("should be default creatable",function(){
        expect(new TTT.Server).toBeDefined();
      });

      it("should provide a scope",function(){
        expect(new TTT.Server.Class).toBeDefined();
      });

      describe("running games",function(){

        beforeEach(function(){
          this.server = new TTT.Server;
        });

        it("should allow players to join a game",function(){
          expect(this.server.join({})).toBeUndefined();
        });

        it("should place a player on deck until paired",function(){
          var player = {};
          var game;
          this.server.join(player, function(_game) {
            game = _game;
          });
          this.server.leave(player,function(){
            expect(game).toBeUndefined();
            complete();
          });
          incomplete();
        });

        it("should return the game to the players",function(){
          var tom = {};
          var jerry = {};
          var toms_game;
          var jerrys_game;
          this.server.join(tom, function(game) {
            toms_game = game;
            if (jerrys_game) {
              expect(toms_game).toBe(jerrys_game);
              complete();
            }
          });
          this.server.join(jerry, function(game) {
            jerrys_game = game;
            if (toms_game) {
              expect(toms_game).toBe(jerrys_game);
              complete();
            }
          });
          incomplete();
        });
        
        it("should count active players",function(){
          this.server.join({});
          this.server.join({});
          this.server.join({});
          this.server.players(function(players){
            expect(players).toBe(3);
            complete();
          });
          incomplete();
        });

      });
    });
  });

})();