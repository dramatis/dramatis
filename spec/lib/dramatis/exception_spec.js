"use strict";
(function($){
  describe("dramatis",function(){
    describe("exceptions",function(){
      
      it("should have the base class",function() {
        expect(Dramatis.Exception).toBeDefined();
      });
      
      it("should have the terminated class",function() {
        expect(Dramatis.Exception.Terminated).toBeDefined();
      });


      it("should have the proper relationships",function() {
        var normal = new Dramatis.Exception.Terminated.Normal();
        expect(normal instanceof Dramatis.Exception.Terminated.Normal).toBe(true);
        expect(normal instanceof Dramatis.Exception.Terminated).toBe(true);
        expect(normal instanceof Dramatis.Exception).toBe(true);
      });


    });
  });
}(jQuery));