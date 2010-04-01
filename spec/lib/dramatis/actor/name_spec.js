"use strict";
(function(){
  describe("dramatis",function(){
    describe("actor",function(){
      describe("name",function(){

        var Runtime = Dramatis.Runtime;
        var Actor = Dramatis.Actor;
        var Name = Actor.Name;
        var any = jasmine.any;

        it("should allow creation of actor names from bare jids",function(){
          expect(new Actor.Name("xmpp:user@host#actor")).toBeDefined();
        });

        it("should allow creation of actor names from full jids",function(){
          expect(new Actor.Name("xmpp:user@host/location#actor")).toBeDefined();
        });

        it("should create remote names for remote actors",function(){
          spyOn(Runtime.Actor.Name,"Remote");
          expect(new Actor.Name("xmpp:user@host/location#actor")).toBeDefined();
          expect(Runtime.Actor.Name.Remote).wasCalled();
        });

        it("should have be extensible",function(){
          var name = new Actor.Name();
          Actor.Name.extend(name, "foo");
          expect(name.foo).toEqual(any(Function));
        });

      });
     });
   });
}());