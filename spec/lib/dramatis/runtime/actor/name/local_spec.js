"use strict";
(function(){
  describe("dramatis",function(){
    describe("runtime",function(){
      describe("actor",function(){
        describe("name",function(){
          describe("local",function(){

            var Actor = Dramatis.Actor;
            var Runtime = Dramatis.Runtime;
            var Local = Runtime.Actor.Name.Local;

            it("should resolve to a uri",function(){
              spyOn(Dramatis.Director.current,"route").
                andReturn(new Runtime.Reactor.Channel.XMPP.Route("user","host","resource"));
              var name = new Actor({});
              var runtime = name.__runtime__;
              expect(runtime.uri()).toMatch(/^xmpp:user@host\/resource#\d+$/);
            });

          });
        });
      });
    });
  });
}());