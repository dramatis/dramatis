jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/ttt/spec_helper.js");

(function(){

  describe("ttt",function(){
    describe("play",function(){

      it("should play a game",function(){
        var server = new TTT.Server;
        var tom = new TTT.Player;
        var jerry = new TTT.Player;

        // join a game ...
        tom.join( server );
        jerry.join( server );
      });

    });
  });

})();