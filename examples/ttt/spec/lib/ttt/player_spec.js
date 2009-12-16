jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/ttt/spec_helper.js");

(function(){

  describe("ttt",function(){
    describe("player",function(){

      it("should be default creatable",function(){
        expect(new TTT.Player).toBeDefined();
      });

      describe("game interaction",function(){

        beforeEach(function(){
          this.server = new TTT.Server;
          this.tom = new TTT.Player;
          this.jerry = new TTT.Player;
        });

        it("game should be null before joining",function(){
          this.tom.game(function(game){
            expect(game).toBeUndefined();
          });
        });

        it("should join a game on a server",function(){
          this.tom.join(this.server);
          this.tom.game(function(game){
            expect(game).toBeDefined();
            complete();
          });
          incomplete();
        });

      });


    });
  });

})();