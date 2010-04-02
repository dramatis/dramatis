"use strict";
(function($){
  describe("dramatis",function(){
    describe("continuation",function(){

      var any = jasmine.any;

      var Actor = Dramatis.Actor;
      var Continuation = Dramatis.Continuation;
      var Continue = Dramatis.Continue;
      var c = Continue;

      it("should be posssible to create a C from an object/method name",
         function(){
           expect(new Continuation([new Actor(),"method"])).toBeDefined();
         });

      it("should be posssible to call an object/method C",
         function(){
           var o = new Actor({method: function method(arg) {
             expect(arg).toBe("xyzzy");
             complete();
           }});
           var c = new Continuation([o,"method"]);
           c.value.call("xyzzy");
           incomplete();
         });

      it("should wrap with a Continue if called w/o new",function() {
        var cont = c({});
        expect(cont).toEqual(any(Continue));
      });

      it("should create a continuation from fns",function() {
        var cx = c({
          value: function(){},
          exception: function() {},
          timeout: function() {}
        });
        expect(cx).toEqual(any(Continue));
      });

      it("calling value should call the value callback",function() {
        var c = new Continuation({
          value: function(value) {
            expect(value).toBe(0xdeadbeef);
            complete();
          },
          exception: function() {
            expect(true).toBe(false);
          },
          timeout: function() {
            expect(true).toBe(false);
          }
        });
        c.value.call(0xdeadbeef);
        incomplete();
      });

    });
  });
}(jQuery));