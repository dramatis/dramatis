"use strict";
(function(){
  describe("dramatis",function(){
    describe("runtime",function(){
      describe("reactor",function(){
        describe("channel",function(){
          describe("xmpp",function(){

            var Reactor = Dramatis.Runtime.Reactor;
            var Channel = Reactor.Channel;
            var XMPP = Reactor.Channel.XMPP;
            var BadURL = Channel.BadURL;
            var NoConnection = Channel.NoConnection;

            beforeEach(function(){
              this.bosh_url = "bosh://host:port/http-bind/user:password@vhost";
            });

            it("should fail if xmpp can't connect",function(){
              var url = this.bosh_url;
              spyOn(Channel, "XMPP").andCallFake(function(url){
                expect(url).toBe(url);
                throw new NoConnection(url);
              });
              expect(function(){(new Channel(url));}).toThrow(jasmine.any(NoConnection));
            });

            describe("able to connect",function(){

              beforeEach(function(){
                spyOn(Strophe,"Connection").andCallFake(Strophe.Mock.Connection.Good);
              });

              it("should parse url to appropriate fields",function(){
                var xmpp = new XMPP(this.bosh_url);
                expect(xmpp.service).toBe("http://host:port/http-bind");
                expect(xmpp.jid).toBe("user@vhost");
                expect(xmpp.password).toBe("password");
              });

              it("should call callback if xmpp can connect",function(){
                var url = this.bosh_url;
                (new Channel(url,function(channel){
                  expect(channel).toBeDefined();
                  complete();
                }));
                incomplete();
              });

            });
            
            describe("unable to connect",function(){

              beforeEach(function(){
                spyOn(Strophe,"Connection").andCallFake(Strophe.Mock.Connection.Bad);
              });

              it("should call callback if xmpp cannot connect",function(){
                var url = this.bosh_url;
                (new Channel(url,function(channel){
                  expect("should not be called").toBeUndefined();
                },function(reason){
                  complete();
                }));
                incomplete();
              });

            });
            
            describe("same domain proxy connection",function(){
              
              beforeEach(function(){
                spyOn(Strophe,"Connection").andCallFake(Strophe.Mock.Connection.SameDomain);
              });

              it("should call callback if xmpp connect via same domain negotiation",function(){
                pending();
                var url = this.bosh_url;
                (new Channel(url,function(channel){
                  complete();
                },function(reason){
                  expect("should not be called").toBeUndefined();
                }));
                incomplete();
              });
              
            });

          });
        });
      });
    });
  });
}());