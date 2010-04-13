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

        it("should jsonize to something reasonable",function(){
          spyOn(Dramatis.Director.current,"route").andReturn(new Runtime.Reactor.Channel.XMPP.Route("user","host","resource"));
          var actor = new Actor({});
          var json = JSON.stringify(actor);
          expect(json).toMatch("Dramatis.Runtime.Actor.Name.Remote");
          expect(json).toMatch("xmpp:user@host/resource");
        });

        it("should coerce to a uri",function(){
          spyOn(Dramatis.Director.current,"route").andReturn(new Runtime.Reactor.Channel.XMPP.Route("user","host","resource"));
          var actor = new Actor({});
          var uri = Actor.Name.uri(actor);
          expect(uri).toMatch(/^xmpp:user@host\/resource#\d+$/);
        });

      });
     });
   });
}());