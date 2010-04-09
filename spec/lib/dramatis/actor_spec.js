"use strict";
(function($){
  describe("dramatis",function(){
    describe("actor",function(){

      var Actor = Dramatis.Actor;
      var Name = Actor.Name;
      var any = jasmine.any;
      var c = Dramatis.Continue;

      it("should be possible to create bare actors from a behavior",function(){
        expect(new Actor({})).toBeDefined();
      });

      it("should attach behaviors",function(){
        var behavior = {};
        (new Actor(behavior));
        expect(behavior.__dramatis__).toBeDefined();
      });

      it("should be represented as an actor name",function(){
        expect(new Actor({})).toEqual(any(Name));
      });

      it("should have methods on the name that reflect the behavior methods",
         function(){
        var behavior = { a: function(){}, b: function(){} };
        var actor = new Actor(behavior);
        expect(actor).toEqual(any(Name));
        expect(actor.a).toBeDefined();
        expect(actor.b).toBeDefined();
        expect(actor.c).toBeUndefined();
      });

      it("should call methods when method is called on name",function(){
        var actor = new Actor({
          foo: function() {
            complete();
          }
        });
        actor.foo();
        incomplete();
      });

      it("calling the name method should call the (right) actor method (type version)",function(){
        incomplete();
        incomplete();
        var methods = { a: function(p){
          expect(p).toBe("a");
          complete();
        }, b: function(p){
          expect(p).toBe("b");
          complete();
        } };
        var Type = new Actor.Type(function(){
          Dramatis.Actor.Behavior.call(this,arguments);
        }, methods);
        var instance = new Type();
        instance.a("a");
        instance.b("b");
      });

      it("should have an enqueue method",function(){
        var name = new Actor({});
        expect(name.__runtime__.actor().enqueue).toBeDefined();
      });

      it("should enqueue to the director on first enquue",function(){
        var director = Dramatis.Director.current;
        spyOn(director,"run");
        var name = new Actor({a:function(){}});
        name.a();
        expect(director.run).wasCalled();
      });


      describe("exceptions",function(){

        it("should call raise on actors on uncaught exceptions", function(){
          spyOn(Dramatis.Actor.prototype,"raise");
          var actor = new Actor({a: function(){var a;return a.b;}});
          actor.a();
          expect(Dramatis.Actor.prototype.raise).wasCalled();
        });

        it("should call abort on caught raise actors on uncaught exceptions", function(){
          spyOn(Dramatis.Actor.prototype,"abort");
          var actor = new Actor({a: function(){var a;return a.b;}});
          actor.a();
          expect(Dramatis.Actor.prototype.abort).wasCalled();
        });

        it("should call termiante on caught raise actors on uncaught exceptions", function(){
          spyOn(Dramatis,"error");
          spyOn(Dramatis.Actor.prototype,"terminate");
          var actor = new Actor({a: function(){var a;return a.b;}});
          actor.a();
          expect(Dramatis.error).wasCalled();
          expect(Dramatis.Actor.prototype.terminate).wasCalled();
        });

        it("should throw an error to the caller on failure", function(){
          spyOn(Dramatis,"error");
          var actor = new Actor({a: function(){var a;return a.b;}});
          actor.a(c({
            exception: [actor, function(e){
              expect(e).toEqual(any(Dramatis.Exception.Terminated));
              expect(Dramatis.error).wasCalled();
              complete();
            }]
          }));
          incomplete();
        });

      });

      describe("behavior",function(){
        it("should be defined", function(){
          expect(Actor.behavior).toBeDefined();
        });

        it("should return behaviors for local actors", function(){
          var behavior = ({});
          var actor = new Actor(behavior);
          expect(Actor.behavior(actor)).toBe(behavior);
        });

        it("should return undefined for remote actors", function(){
          var actor = new Actor.Name("xmpp:user@host/resource#id");
          expect(Actor.behavior(actor)).toBeUndefined();
        });
      });

      describe("id",function(){
        it("should provide an id fn",function(){
          expect(Actor.id).toBeDefined();
        });

        it("should for work actor names",function(){
          var actor = new Actor({a: function(){var a;return a.b;}});
          expect(typeof Actor.id(actor) === "number").toBe(true);
        });

        it("should for work behaviors",function(){
          var actor = new Actor({a: function(){
            expect(typeof Actor.id(this) === "number").toBe(true);
            complete();
          }});
          actor.a();
          incomplete();
        });
      });

      describe("termination",function() {
        it("should be happy with behaviors that don't have terminate",function() {
          spyOn(Dramatis,"error");
          var actor = new Actor({a: function(){var a;return a.b;}});
          Actor.terminate(actor);
          expect(Dramatis.error).wasNotCalled();
        });

        it("should call terminate on behaviors at termination",function() {
          var Cls = new Actor.Type(function(){
            Actor.Behavior.call(this,arguments);
            Actor.on_terminate(this, "on_terminate");
          }, {
            on_terminate: function(reason) {
              expect(reason).toEqual(any(Dramatis.Exception.Terminated.Normal));
              complete();
            }
          });
          var actor = new Cls();
          Actor.terminate(actor);
          incomplete();
        });


        it("should not deliver tasks to terminated actors", function(){
          pending();
        });

      });

      describe("pubsub",function() {

        it("should be possible to subscribe to lifecycle events",function(){
          var actor = new Actor({a: function(){var a;return a.b;}});
          Dramatis.extend(this, Dramatis.Subscriber);
          this.subscribe({to: Actor.lifecycle(actor), call:"method"});
          expect(true).toBe(true);
        });
        
        it("should call subscriber on failiure",function(){
          spyOn(Dramatis,"error");
          var actor = new Actor({a: function(){var a;return a.b;}});
          Dramatis.extend(this, Dramatis.Subscriber);
          this.subscribe({to: Actor.lifecycle(actor), call:  function(an, reason){
            expect(an.equals(actor));
            expect(reason).toEqual(any(Dramatis.Exception.Terminated));
            expect(Dramatis.error).wasCalled();
            complete();
          }});
          actor.a();
          incomplete();
        });

        it("should call subscriber on normal termination",function(){
          var actor = new Actor({a: function(){
            Actor.terminate(this);
          }});
          Dramatis.extend(this, Dramatis.Subscriber);
          this.subscribe({to: Actor.lifecycle(actor), call:  function(an, reason){
            expect(an.equals(actor));
            expect(reason).toEqual(any(Dramatis.Exception.Terminated.Normal));
            complete();
          }});
          actor.a();
          incomplete();
        });

      });

    });
  });
}(jQuery));