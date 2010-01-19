jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");

(function($){
  describe("dramatis",function(){
    describe("actor",function(){

      var Actor = Dramatis.Actor;
      var Name = Actor.Name;
      var any = jasmine.any;

      it("should be possible to create bare actors from a behavior",function(){
        expect(new Actor({})).toBeDefined();
      });

      it("should attach behaviors",function(){
        var behavior = {};
        new Actor(behavior);
        expect(behavior.__dramatis__).toBeDefined();
      });

      it("should be represented as an actor name",function(){
        expect(new Actor({})).toEqual(any(Name));
      });

      it("should have methods on the name that reflect the behavior methods",function(){
        var behavior = { a: function(){}, b: function(){} };
        var actor = new Actor(behavior);
        expect(actor).toEqual(any(Name));
        expect(actor.a).toBeDefined();
        expect(actor.b).toBeDefined();
        expect(actor.c).toBeUndefined();
      });

      it("should call methods when method is called on name",function(){
        var actor = new Actor({
          foo: function() {
            complete();
          }
        });
        actor.foo();
        incomplete();
      });

      it("calling the name method should call the (right) actor method (type version)",function(){
        incomplete();
        incomplete();
        var methods = { a: function(p){
          expect(p).toBe("a");
          complete();
        }, b: function(p){
          expect(p).toBe("b");
          complete();
        } };
        var type = new Actor.Type(function(){
          Dramatis.Actor.Behavior.call(this,arguments);
        }, methods);
        var instance = new type;
        instance.a("a");
        instance.b("b");
      });

    });
  });
})(jQuery);