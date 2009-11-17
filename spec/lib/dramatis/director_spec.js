jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");

(function(){
  describe("dramatis",function(){
    describe("director",function(){

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
        spyOn(Dramatis.Director.current.reactor, "disconnect");
        var s = "bosh://host:port/path/user:password@vhostname";
        Dramatis.Director.current.disconnect("bosh://host:port/path/user:password@vhostname");
        expect(Dramatis.Director.current.reactor.disconnect).wasCalledWith(s);
      });

      if("should have a reset",function(){
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

        it("should call callback on success",function(){
          spyOn(Dramatis.Director.current.reactor, "connect").andCallFake(function(){
          });
          var s = "bosh://host:port/path/user:password@vhostname";
          Dramatis.Director.current.disconnect("bosh://host:port/path/user:password@vhostname");
          expect(Dramatis.Director.current.reactor.disconnect).wasCalledWith(s);
        });


      });

    });
  });
})();