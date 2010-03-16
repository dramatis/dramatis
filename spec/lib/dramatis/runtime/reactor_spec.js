"use strict";
(function(){
  describe("dramatis",function(){
    describe("runtime",function(){
      describe("reactor",function(){

        var Reactor = Dramatis.Runtime.Reactor;

        describe("connect",function(){

          beforeEach(function(){
            this.reactor = new Reactor({});
            this.good_url = "bosh://host:port/http-bind/user:password@vhost";
          });
          
          it("should have a connections array that starts empty",function(){
            expect(this.reactor.connections.length).toBe(0);
          });

          it("should add a new channel object on connect",function(){
            var url = this.good_url;
            spyOn(Reactor, "Channel").andCallFake(function(url,good,bad){
              expect(url).toBe(url);
              good({});
            });
            this.reactor.connect(url);
            expect(this.reactor.connections.length).toBeGreaterThan(0);
          });

        });

      });
    });
  });
}());