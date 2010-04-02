"use strict";
(function($){
  describe("dramatis",function(){
    describe("runtime",function(){
      describe("task",function(){

            var Task = Dramatis.Runtime.Task;

        it("should convert to json",function(){
          var task = new Task();
          expect(JSON.stringify(task)).toBeDefined();
        });

        it("should roundrip through json",function(){
          var task = new Task( "name", "method", [ "args" ], "continuation" );
          var json = JSON.stringify(task);
          var new_task = Dramatis.JSON.parse(json);
          expect(new_task).toEqual(task);
        });

      });
    });
  });
}(jQuery));