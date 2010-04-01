"use strict";
(function(){
  describe("dramatis",function(){
    describe("runtime",function(){
      describe("reactor",function(){
        describe("channel",function(){
          describe("xmpp",function(){

            var global = (function(){return this;}());

            var Reactor = Dramatis.Runtime.Reactor;
            var Channel = Reactor.Channel;
            var XMPP = Reactor.Channel.XMPP;
            var BadURL = Channel.BadURL;
            var NoConnection = Channel.NoConnection;

            beforeEach(function(){
              this.bosh_url = "bosh://host:port/http-bind/user:password@vhost/resource";
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
                expect(xmpp.jid).toBe("user@vhost/resource");
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

              it("should have a uri accessor if it can connect",function(){
                var url = this.bosh_url;
                (new Channel(url,function(channel){
                  expect(channel).toBeDefined();
                  expect(channel.uri()).toBe("xmpp:user@vhost/resource");
                  complete();
                }));
                incomplete();
              });

              it("should not call connect fail after connection complete",function(){
                var url = this.bosh_url;
                (new Channel(url,function(channel){
                  channel.connection.fail();
                  global.setTimeout(function(){
                    complete();
                  },0);
                },function(){
                  expect(true).toBe(false);
                }));
                incomplete();
              });

              it("should call disconnect callbacks on connection lost",function(){
                var url = this.bosh_url;
                (new Channel(url,function(channel){
                  channel.on_disconnect(function(){
                    complete();
                  });
                  channel.connection.fail();
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

            describe("jid parsing",function(){
              it("should parse xmpp urls",function(){
                expect(XMPP.parse_url("xmpp:user@host/resource#actor")).toBeDefined();
              });

              it("should parse xmpp urls to the proper route and name",function(){
                expect(XMPP.parse_url("xmpp:user@host/resource#actor")).toEqual({
                  route: new XMPP.Route("user","host","resource"),
                  actor: "actor"
                });
              });

              it("should parse xmpp urls to the proper route and name w/resources",function(){
                expect(XMPP.parse_url("xmpp:user@host#actor")).toEqual({
                  route: new XMPP.Route("user","host"),
                  actor: "actor"
                });
              });

              it("should convert routes to string w/o resources",function() {
                expect(new XMPP.Route("user","host","resource")+"").
                  toBe("xmpp:user@host/resource");
              });

              it("should convert routes to string w/resources",function() {
                expect(new XMPP.Route("user","host")+"").
                  toBe("xmpp:user@host");
              });

              it("should ignore non-xmpp urls",function(){
                expect(XMPP.parse_url("http:")).toBeUndefined();
              });
              
              it("should have a send method",function(){
                expect(XMPP.prototype.send).toBeDefined();
              });
            });
          });
        });
      });
    });
  });
}());