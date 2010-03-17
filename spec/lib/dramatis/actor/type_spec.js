"use strict";
(function(){
  describe("dramatis",function(){
    describe("actor",function(){
      describe("type",function(){

        var Actor = Dramatis.Actor;
        var Name = Actor.Name;
        var any = jasmine.any;

        it("should be possible to create new actor types",function(){
          expect(new Actor.Type(function(){})).toBeDefined();
        });

        it("should be represented as an actor name",function(){
          var Type = new Actor.Type(function(){});
          expect(new Type()).toEqual(any(Name));
        });
        
        it("should have a name type with the appropriate methods",function(){
          var methods = { a: function(){}, b: function(){} };
          var type = new Actor.Type(function(){},methods);
          var name = type.Name;
          expect(name).toBeDefined();
          expect(name.prototype.a).toBeDefined();
          expect(name.prototype.b).toBeDefined();
          expect(name.prototype.c).toBeUndefined();
        });
        
        it("should call the behavior constructor when constructed",function(){
          incomplete();
          var called = false;
          var Type = new Actor.Type(function(){
            called = true;
            complete();
          });
          new Type();
        });
        
        it("should pass the actor interface to the constructor",function(){
          incomplete();
          var Type = new Actor.Type(function(__dramatis__){
            expect(__dramatis__).toEqual(any(Actor.Interface));
            complete();
          });
          new Type();
        });

        it("should call behavior constructor with correct arguments",function(){
          incomplete();
          var Type = new Actor.Type(function(){
            Array.prototype.shift.call(arguments);
            expect(arguments).toEqual( [ 1, 2, "a", { b: "c" } ] );
            complete();
          });
          new Type( 1, 2, "a", { b: "c" } );
        });

        it("should call the behavior methods when name methods called",function(){
          incomplete();
          var called = false;
          var Type = new Actor.Type(function(){}, {
            foo: function() {
              called = true;
              complete();
            }
          });
          (new Type()).foo();
        });
        
        it("should call behavior constructor with correct arguments",function(){
          incomplete();
          var Type = new Actor.Type(function(){}, {
            foo: function() {
              expect(arguments).toEqual( [ 1, 2, "a", { b: "c" } ] );
              complete();
            }
          });
          (new Type().foo(1, 2, "a", { b: "c" }));
        });

      });
     });
   });
}());