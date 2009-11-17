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
          
          xit("should add a new channel object on connect",function(){
          });

        });

      });
    });
  });
})();