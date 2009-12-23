jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");

(function($){
  describe("dramatis",function(){
    describe("pubsub",function(){

      beforeEach(function(){
        this.subscriber = function(){};
        $.extend(this.subscriber.prototype,Dramatis.Subscriber.prototype);
        this.publisher = function(){};
        $.extend(this.publisher.prototype,Dramatis.Publisher.prototype);
        this.sub = new this.subscriber;
        this.pub = new this.publisher;
      });

      it("should be possible to subscribe to a publisher",function(){
        this.sub.subscribe({to: this.pub, call: "method"});
      });

      it("should receive callbacks on publisher changes",function(){
        var hash = {a: "b"};
        this.sub.subscribe({to: this.pub, call: "method"});
        this.sub.method = function method(state) {
          expect(state).toEqual(hash);
          complete();
        };
        this.pub.notify(hash);
        incomplete();
      });

      it("should be possible to unsubscribe");
      it("should not receive callbacks after unsub");

    });

  });
})(jQuery);