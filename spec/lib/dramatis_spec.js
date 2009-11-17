jazrb_root = this.jazrb_root || ".";
include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");

(function(){
  describe("dramatis",function(){

    it("should add connections",function(){
      spyOn(Dramatis.Director.current,"connect");
      var c = "bosh://localhost:5280/http-bind/user:password@localhost";
      expect(Dramatis.connect(c)).toBeUndefined();
      expect(Dramatis.Director.current.connect).wasCalledWith(c);
    });

    it("should remove connections",function(){
      spyOn(Dramatis.Director.current,"disconnect");
      var c = "bosh://localhost:5280/http-bind/user:password@localhost";
      expect(Dramatis.disconnect(c)).toBeUndefined();
      expect(Dramatis.Director.current.disconnect).wasCalledWith(c);
    });

  });
})();