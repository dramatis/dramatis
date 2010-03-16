//jazrb_root = this.jazrb_root || ".";
// include(jazrb_root + "/spec/lib/dramatis/spec_helper.js");
"use strict";
(function($){
  describe("dramatis",function(){
    describe("future",function(){

      var window = (function(){return this;}());
      var setTimeout = window.setTimeout;

      var Future = Dramatis.Future;

      it("should exist",function(){
        expect(Future).toBeDefined();
      });

      describe("using",function(){

        it("should callback with no futures",function(){
          incomplete();
          Future.using(function(){
            expect(arguments.length).toBe(0);
            complete();
          });
        });

        it("should not callback w/o futures",function(){
          incomplete();
          var futures = [ new Future(), new Future() ];
          Future.using( futures[0], futures[1], function(){
            expect(false).toBe(true);
          });
          setTimeout(function(){complete();},100);
        });

        it("should callback with correct value of futures (precomputed)",function(){
          incomplete();
          var futures = [ new Future(), new Future() ];
          futures[0].set(1);
          futures[1].set(2);
          Future.using( futures[0], futures[1], function(){
            expect(arguments).toEqual([1,2]);
            complete();
          });
        });

        it("should callback with correct value of futures (postcomputed)",function(){
          incomplete();
          var futures = [ new Future(), new Future() ];
          Future.using( futures[0], futures[1], function(){
            expect(arguments).toEqual([1,2]);
            complete();
          });
          setTimeout(function(){futures[1].set(2);},50);
          setTimeout(function(){futures[0].set(1);},100);
        });

        it("should callback with correct value of futures (hybrid)",function(){
          incomplete();
          var futures = [ new Future(), new Future() ];
          futures[1].set(2);
          Future.using( futures[0], futures[1], function(){
            expect(arguments).toEqual([1,2]);
            complete();
          });
          setTimeout(function(){futures[0].set(1);},100);
        });

      });
      
      describe("with actor names",function(){

        it("should be returned when calling an actor name method with a future parameter",function(){
          incomplete();
          var actor = new Dramatis.Actor({
            foo: function(){ return "foo"; }
          });
          var result = actor.foo(Future);
          result.subscribe(function(v){
            expect(v).toBe("foo");
            complete();
          });
        });
        
      });

    });
  });
}(jQuery));