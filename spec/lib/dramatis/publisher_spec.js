//jazrb_root = this.jazrb_root || ".";
// include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");
"use strict";
(function($){
  describe("dramatis",function(){
    describe("publisher",function(){

      it("should be posssible to mix publisher in",function(){
        var cls = function(){};
        $.extend(cls.prototype,Dramatis.Publisher.prototype);
      });

      it("should accept subscription requests w/o options", function() {
        var Cls = function(){};
        $.extend(Cls.prototype,Dramatis.Publisher.prototype);
        var pub =  new Cls();
        pub.add_subscription( {} );
      });

      it("should accept subscription cancelations");
      it("should call callbacks on state changes");

    });

  });
}(jQuery));