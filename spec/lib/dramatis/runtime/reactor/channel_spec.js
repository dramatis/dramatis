"use strict";
(function(){
  describe("dramatis",function(){
    describe("runtime",function(){
      describe("reactor",function(){
        describe("channel",function(){

          var Reactor = Dramatis.Runtime.Reactor;
          var Channel = Reactor.Channel;
          var BadURL = Channel.BadURL;

          beforeEach(function(){
            this.bosh_url = "bosh://host:port/http-bind/user:password@vhost";
          });

          it("should call bad on unparsable url",function(){
            var good = jasmine.createSpy("good");
            var bad = jasmine.createSpy("bad");
            expect(function(){(new Channel("xbosh://...."));}).toThrow(jasmine.any(BadURL));
            expect(good).wasNotCalled();
            expect(bad).wasNotCalled();
          });

          it("should hand off BOSH urls to XMPP",function(){
            spyOn(Channel, "XMPP");
            expect(new Channel("bosh://....")).toBeDefined();
            expect(Channel.XMPP).wasCalled();
          });

        });

      });
    });
  });
}());