//jazrb_root = this.jazrb_root || ".";
//include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");
"use strict";
(function($){
  describe("dramatis",function(){
    describe("pubsub",function(){

      var Actor = Dramatis.Actor;

      beforeEach(function(){
        this.Subscriber = new Actor.Type( new Dramatis.Class( [ Dramatis.Subscriber ] ) );
        this.Publisher = new Actor.Type( new Dramatis.Class( [ Dramatis.Publisher ] ) );
        this.sub = new this.Subscriber();
        this.pub = new this.Publisher();
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

      it("should receive an initial state", function() {
        var hash = {a: "b"};
        this.sub.method = function method(state) {
          expect(state).toEqual(hash);
          complete();
        };
        this.pub.update = function update() {
          this.notify(hash);
        };
        this.sub.subscribe({to: this.pub, call: "method"});
        incomplete();
      });

      it("should not receive an initial state if up to date", function() {
        pending();
      });

      it("should only send state to new object on init request");

      it("should be possible to unsubscribe", function() {
        pending();
      });

      it("should not receive callbacks after unsub");

    });

  });
}(jQuery));