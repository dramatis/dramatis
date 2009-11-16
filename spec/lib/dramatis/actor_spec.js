jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");

(function(){
  describe("dramatis",function(){
    describe("actor",function(){

      var Actor = Dramatis.Actor;
      var Name = Actor.Name;
      var any = jasmine.any;

      it("should be possible to create bare actors from a behavior",function(){
        expect(new Dramatis.Actor({})).toBeDefined();
      });

      it("should be represented as an actor name",function(){
        expect(new Actor({})).toEqual(any(Name));
      });

    });
  });
})();