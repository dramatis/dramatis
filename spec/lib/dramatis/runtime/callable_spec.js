"use strict";
(function(){
  describe("dramatis",function(){
    describe("runtime",function(){
      describe("callable",function(){

        var any = jasmine.any;

        var Callable = Dramatis.Runtime.Callable;

        it("should create a function callable from a function", function(){
          var callable = new Callable(new Dramatis.Actor({}), function(){});
          expect(callable).toEqual(any(Callable.Function));
        });

      });

    });
  });
}());