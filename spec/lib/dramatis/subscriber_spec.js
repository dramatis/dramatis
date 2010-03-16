// jazrb_root = this.jazrb_root || ".";
// include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");
"use strict";
(function($){
  describe("dramatis",function(){
    describe("subscriber",function(){
      it("should be posssible to mix subscriber in",function(){
        var Cls = function(){};
        $.extend(Cls.prototype,Dramatis.Subscriber.prototype);
      });

    });
  });
}(jQuery));