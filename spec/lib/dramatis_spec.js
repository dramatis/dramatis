"use strict";
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

    it("should call callback on succesful connect",function(){
      spyOn(Strophe,"Connection").andCallFake(Strophe.Mock.Connection.Good);
      var c = "bosh://localhost:5280/http-bind/user:password@localhost";
      Dramatis.connect(c,function good_callback(){
        complete();
      },function failed_callback(){
        expect("should not be called").toBeUndefined();
      });
      incomplete();
    });

    it("should call callback on unsuccesful connect",function(){
      spyOn(Strophe,"Connection").andCallFake(Strophe.Mock.Connection.Bad);
      var c = "bosh://localhost:5280/http-bind/user:password@localhost";
      Dramatis.connect(c,function good_callback(){
        expect("should not be called").toBeUndefined();
      },function failed_callback(){
        complete();
      });
      incomplete();
    });

  });
}());