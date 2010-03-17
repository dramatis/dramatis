"use strict";
(function(){
  describe("ttt",function(){
    describe("class",function(){

      it("should be default creatable",function(){
        expect(new Dramatis.Class()).toBeDefined();
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

      it("should add a toString if given a fn name", function() {
        var Cls = new Dramatis.Class( function TestName() {} );
        expect(Cls+"").toBe("Dramatis.TestName");
      });

      it("should add a toString if given a name",function() {
        var Cls = new Dramatis.Class( {}, {name: "TestName"} );
        expect(Cls+"").toBe("TestName");
      });
      
      it("should not add a toString if not given a name",function() {
        var fn = function(){};
        var Cls = new Dramatis.Class( fn );
        expect(Cls+"").toBe(fn+"");
      });

      it("should handle a mixin-only arg list",function(){
        var Mixin = new Dramatis.Class( { mixin: function(){} } );
        var Cls = new Dramatis.Class( [ Mixin ] );
        expect(Cls.prototype.mixin).toBeDefined();
      });

    });
  });
}());