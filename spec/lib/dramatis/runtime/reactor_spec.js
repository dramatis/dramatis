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

          it("should have a send method",function(){
            expect(this.reactor.send).toBeDefined();
          });

          describe("with an xmpp channel",function() {
            
            beforeEach(function(){
              this.url = this.good_url;
              spyOn(Reactor, "Channel").andCallFake(function(url,good,bad){
                expect(url).toBe(url);
                good({});
              });
            });
            
            it("should add a new channel object on connect",function(){
              this.reactor.connect(this.url);
              expect(this.reactor.connections.length).toBeGreaterThan(0);
            });

          });

        });


        it("should route objects via task name route",function(){
          var reactor = new Reactor({});
          var bosh =  "bosh://host:port/http-bind/user:password@vhost";
          var Name = Dramatis.Actor.Name;
          var XMPP = Reactor.Channel.XMPP;
          var name = new Name("xmpp:user@host/location#actor");
          Name.extend(name,"a");
          spyOn(Strophe,"Connection").andCallFake(Strophe.Mock.Connection.Good);
          spyOn(XMPP.prototype,"send");
          reactor.connect(bosh, function() {
            reactor.send(name.__runtime__.route, ({}));
            expect(XMPP.prototype.send).wasCalled();
            complete();
          });
          incomplete();
        });

      });

    });
  });
}());