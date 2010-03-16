"use strict";
(function(){
  describe("dramatis",function(){
    describe("actor",function(){
      describe("behavior",function(){

        var Actor = Dramatis.Actor;
        var Name = Actor.Name;
        var any = jasmine.any;

        it("should accept the actor interface arg in the constructor",function(){
          incomplete();
          var Type = new Actor.Type(function(/*..*/){
            var len = arguments.length; 
            Actor.Behavior.call(this,arguments);
            expect(len-arguments.length).toBe(1);
            complete();
          });
          (new Type());
        });

        it("should bind the behavior in the base constructor",function(){
          incomplete();
          var Type = new Actor.Type(function(/*..*/){
            var len = arguments.length; 
            Actor.Behavior.call(this,arguments);
            expect(this.__dramatis__.actor.behavior).toBe(this);
            complete();
          });
          (new Type());
        });

      });
    });
  });
}());