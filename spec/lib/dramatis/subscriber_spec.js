"use strict";
(function($){
  describe("dramatis",function(){
    describe("subscriber",function(){
      it("should have the right name",function(){
        expect(Dramatis.Subscriber+"").toBe("Dramatis.Subscriber");
      });
      it("should be posssible to mix subscriber in",function(){
        var Cls = function(){};
        $.extend(Cls.prototype,Dramatis.Subscriber.prototype);
      });

    });
  });
}(jQuery));