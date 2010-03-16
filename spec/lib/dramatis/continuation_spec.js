// jazrb_root = this.jazrb_root || ".";
// include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");
"use strict";
(function($){
  describe("dramatis",function(){
    describe("continuation",function(){

      var Actor = Dramatis.Actor;
      var Continuation = Dramatis.Continuation;

      it("should be posssible to create a C from an object/method name",
         function(){
           expect(new Continuation(new Actor(),"method")).toBeDefined();
         });

      it("should be posssible to call an object/method C",
         function(){
           var o = new Actor({method: function method(arg) {
             expect(arg).toBe("xyzzy");
             complete();
           }});
           var c = new Continuation(o,"method");
           c.call("xyzzy");
           incomplete();
         });

    });
  });
}(jQuery));