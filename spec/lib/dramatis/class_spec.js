jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");

(function(){

  describe("ttt",function(){
    describe("class",function(){

      it("should be default creatable",function(){
        expect(new Dramatis.Class).toBeDefined();
      });

      it("should default to the name of the constructor",function(){
        var cls = new Dramatis.Class(function Player(){});
        expect(cls+"").toBe("Dramatis.Player");
      });

      it("should copy methods",function(){
        var methods = { a: function(){}, b: function(){} };
        var cls = new Dramatis.Class(function Player(){},methods);
        expect(cls.prototype.a).toBe(methods.a);
        expect(cls.prototype.b).toBe(methods.b);
      });

      it("should provide a Subscope method",function(){
        var Player = new Dramatis.Class(function Player(){});
        new Dramatis.Class.Subscope(Player);
        var View = new Player.Class(function View(){});
        expect(View+"").toBe("Dramatis.Player.View");
      });

    });
  });

})();