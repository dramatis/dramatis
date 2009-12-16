jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/ttt/spec_helper.js");

(function(){

  describe("ttt",function(){
    describe("server",function(){
      describe("view",function(){

        it("should be creatable from a server",function(){
          expect(new TTT.Server.View(new TTT.Server)).toBeUndefined(); // toBeDefined();
        });

      });
    });
  });

})();