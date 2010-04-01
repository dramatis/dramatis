"use strict";
(function(){
  describe("dramatis",function(){
    describe("director",function(){

      var Actor = Dramatis.Actor;
      var any = jasmine.any;

      it("should provide access to the current director",function(){
        expect(Dramatis.Director.current).toBeDefined();
      });

      it("should provide access to the behavior, not the name",function(){
        expect(Dramatis.Director.current).toEqual(any(Dramatis.Director));
        expect(Dramatis.Director.current).toNotEqual(any(Dramatis.Actor.Name));
      });

      it("should have a connect method",function(){
        expect(Dramatis.Director.current.connect).toBeDefined();
      });

      it("should have a disconnect method",function(){
        expect(Dramatis.Director.current.connect).toBeDefined();
      });

      it("should pass disconnect to the reactor",function(){
        var reactor = Dramatis.Director.current.reactor();
        spyOn(reactor, "disconnect");
        var s = "bosh://host:port/path/user:password@vhostname";
        Dramatis.Director.current.disconnect("bosh://host:port/path/user:password@vhostname");
        expect(reactor.disconnect).wasCalledWith(s);
      });

      it("should have a reset", function(){
        pending();
        spyOn(Dramatis.Director.current.reactor, "destroy");
        spyOn(Dramatis.Director.current, "destroy");
        Dramatis.Director.reset();
        expect(Dramatis.Director.current.destroy).wasCalled();
        expect(Dramatis.Director.current.reactor.destroy).wasCalled();
      });

      describe("connection states",function(){
        beforeEach(function(){
          Dramatis.Director.reset();
        });
      });

      it("should deliver local tasks",function(){
        var actor = new Actor({a: function(){}});
        spyOn(actor,"a");
        actor.a();
        expect(actor.a).wasCalled();
      });

      it("should forward remote tasks to remote directors",function(){
        var name = new Actor.Name("xmpp:user@host#actor");
        var bosh =  "bosh://host:port/http-bind/user:password@vhost";
        Actor.Name.extend(name, "a");
        spyOn(Strophe,"Connection").andCallFake(Strophe.Mock.Connection.Good);
        spyOn(Dramatis.Runtime.Reactor.Channel.XMPP.prototype,"send");
        Dramatis.Director.current.connect(bosh, function() {
          name.a();
          expect(Dramatis.Runtime.Reactor.Channel.XMPP.prototype.send).wasCalled();
          complete();
        });
        incomplete();
      });

    });
  });
}());