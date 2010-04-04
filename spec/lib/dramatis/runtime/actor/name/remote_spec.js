"use strict";
(function(){
  describe("dramatis",function(){
    describe("runtime",function(){
      describe("actor",function(){
        describe("name",function(){
          describe("remote",function(){

            var Actor = Dramatis.Actor;
            var Runtime = Dramatis.Runtime;
            var Remote = Runtime.Actor.Name.Remote;

            it("should have a send method",function(){
              var name = new Actor.Name("xmpp:user@host#actor");
              expect(name.__runtime__.send).toBeDefined();
            });

          });
        });
      });
    });
  });
}());