"use strict";
(function(){
  describe("dramatis",function(){
    describe("actor",function(){
      describe("name",function(){
        describe("type",function(){

          var Actor = Dramatis.Actor;
          var Name = Actor.Name;
          var any = jasmine.any;

          it("should be possible to create new actor name types",function(){
            expect(new Actor.Name.Type()).toBeDefined();
          });

          it("should be a subclass of the base name type",function(){
            var Type = new Actor.Name.Type();
            expect(new Type({id:0})).toEqual(any(Name));
          });
          
          it("should reflect the actor methods on the prototype",function(){
            var methods = { a: function(){}, b: function(){} };
            var actor = function(){};
            actor.prototype = methods;
            var type = new Actor.Name.Type(actor);
            expect(type.prototype.a).toBeDefined();
            expect(type.prototype.b).toBeDefined();
            expect(type.prototype.c).toBeUndefined();
          });
          
          it("should reflect the actor methods on name instances",function(){
            var methods = { a: function(){}, b: function(){} };
            var actor = function(){};
            actor.prototype = methods;
            var Type = new Actor.Name.Type(actor);
            var name = new Type({id:0});
            expect(name.a).toBeDefined();
            expect(name.b).toBeDefined();
            expect(name.c).toBeUndefined();
          });
          
          it("instances should reflect the constructor",function(){
            var Type = new Actor.Name.Type();
            var name = new Type({id:0});
            expect(name.constructor).toBe(Type);
          });
          
        });
      });
    });
  });
}());