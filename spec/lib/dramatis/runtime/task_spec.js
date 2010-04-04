"use strict";
(function($){
  describe("dramatis",function(){
    describe("runtime",function(){
      describe("task",function(){

        var Task = Dramatis.Runtime.Task;
        var Callable = Dramatis.Runtime.Callable;

        it("should convert to json",function(){
          var task = new Task();
          expect(JSON.stringify(task)).toBeDefined();
        });

        it("should roundrip through json",function(){
          var name = new Dramatis.Actor({});
          var task = new Task(new Callable([name, "method"]), [ "args" ], "continuation");

          spyOn(Strophe,"Connection").andCallFake(Strophe.Mock.Connection.Good);
          Dramatis.Director.current.
            connect("bosh://host:port/http-bind/user:password@vhost",
                    function(){
                      var json = JSON.stringify(task);
                      var new_task = Dramatis.JSON.parse(json);
                      // Force resolution ...
                      new_task.callable.name.actor();
                      expect(new_task).toEqual(task);
                      complete();
                    });
          incomplete();
        });

      });
    });
  });
}(jQuery));