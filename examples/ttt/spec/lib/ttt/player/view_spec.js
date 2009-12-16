jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/ttt/spec_helper.js");

(function(){

  describe("ttt",function(){
    describe("player",function(){
      describe("view",function(){

        it("should be creatable from a player",function(){
          expect(new TTT.Player.View(new TTT.Player)).toBeDefined();
        });

      });
    });
  });

})();