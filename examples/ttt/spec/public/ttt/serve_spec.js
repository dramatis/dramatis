this.jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/public/ttt/spec_helper.js");

(function($){

  describe("ttt",function(){
    describe("serve",function(){

      it("should exist",function(){
        expect($("title:contains('Serve')").size()).toEqual(1);
      });

      it("should have a server view",function(){
        expect($("#server").size()).toEqual(1);
        pending();
        expect($("#server").html()).toNotMatch(/^\s*$/);
      });

    });

  });

})(jQuery);