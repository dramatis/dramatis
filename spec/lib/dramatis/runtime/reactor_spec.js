jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");

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
          
          it("should call callback on success",function(){
            var okay = jasmine.createSpy("okay");
            var not_okay = jasmine.createSpy("not okay");
            this.reactor.connect(this.good_url, okay, not_okay);
            expect(okay).wasCalled();
            expect(not_okay).wasNotCalled();
          });

          xit("should add a new channel object on connect",function(){
          });

        });

      });
    });
  });
})();