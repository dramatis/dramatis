jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/ttt/spec_helper.js");

(function(){

  describe("ttt",function ttt(){
    describe("game",function game(){

      it("should be default creatable with players",function(){
        expect(new TTT.Game({},{})).toBeDefined();
      });

      describe("play",function play(){

        beforeEach(function beforeEach(){
          this.tom = {};
          this.jerry = {};
          this.game = new TTT.Game(this.tom,this.jerry);
        });

        it("should ask the first player to move",function(){
          this.tom.move = function move() {
            expect(true).toBe(true);
            complete();
          };
          this.game.start();
          incomplete();
        });

      });
    });
  });

})();