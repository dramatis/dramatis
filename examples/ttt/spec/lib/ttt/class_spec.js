jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/ttt/spec_helper.js");

(function(){

  describe("ttt",function(){
    describe("class",function(){

      it("should be default creatable",function(){
        expect(new TTT.Class).toBeDefined();
      });

      it("should default to the name of the constructor",function(){
        var cls = new TTT.Class(function Player(){});
        expect(cls+"").toBe("TTT.Player");
      });

      it("should copy methods",function(){
        var methods = { a: function(){}, b: function(){} };
        var cls = new TTT.Class(function Player(){},methods);
        expect(cls.prototype.a).toBe(methods.a);
        expect(cls.prototype.b).toBe(methods.b);
      });

      it("should provide a Subscope method",function(){
        var Player = new TTT.Class(function Player(){});
        new TTT.Class.Subscope(Player);
        var View = new Player.Class(function View(){});
        expect(View+"").toBe("TTT.Player.View");
      });

    });
  });

})();