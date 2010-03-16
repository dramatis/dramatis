"use strict";
(function(){
  describe("dramatis",function(){
    describe("actor",function(){
      describe("class",function(){

        var Actor = Dramatis.Actor;
        var Name = Actor.Name;
        var any = jasmine.any;

        it("should be possible to create new actor classes",function(){
          expect(new Actor.Type(function(){})).toBeDefined();
        });

        it("should be represented as an actor name",function(){
          var Cls = new Actor.Type(function(){});
          expect(new Cls()).toEqual(any(Name));
        });
        
      });
     });
   });
}());