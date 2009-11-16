jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");

(function(){
  describe("dramatis",function(){
    describe("actor",function(){
      describe("name",function(){

        var Actor = Dramatis.Actor;
        var Name = Actor.Name;
        var any = jasmine.any;

        it("should be possible to create new actor namees",function(){
          expect(new Actor.Type(function(){})).toBeDefined();
        });

        it("should be represented as an actor name",function(){
          var cls = new Actor.Type(function(){});
          expect(new cls).toEqual(any(Name));
        });
        
      });
     });
   });
})();