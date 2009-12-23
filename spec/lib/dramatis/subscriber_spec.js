jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");

(function($){
  describe("dramatis",function(){
    describe("subscriber",function(){

      it("should be posssible to mix subscriber in",function(){
        var cls = function(){};
        $.extend(cls.prototype,Dramatis.Subscriber.prototype);
      });

    });

  });
})(jQuery);