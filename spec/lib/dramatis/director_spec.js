jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");

(function(){
  describe("dramatis",function(){
    describe("director",function(){

      var any = jasmine.any;

      it("should provide access to the current director",function(){
        expect(Dramatis.Director.current).toBeDefined();
        expect(Dramatis.Director.current).toEqual(any(Dramatis.Actor.Name));
      });

    });
  });
})();