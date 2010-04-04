"use strict";
(function(){
  describe("dramatis",function(){
    describe("runtime",function(){
      describe("actor",function(){
        describe("name",function(){
          describe("base",function(){

            var Actor = Dramatis.Actor;
            var Name = Actor.Name;
            var Runtime = Dramatis.Runtime;
            var Task = Runtime.Task;
            var Base = Runtime.Actor.Name.Base;

            it("should create tasks on call",function(){
              var name = new Actor({foo: function(){}});
              spyOn(name.__runtime__.actor().behavior,"foo");
              spyOn(Runtime,"Task").andCallThrough();
              name.foo();
              expect(Runtime.Task).wasCalled();
              expect(name.__runtime__.actor().behavior.foo).wasCalled();
            });

          });
        });
      });
    });
  });
}());