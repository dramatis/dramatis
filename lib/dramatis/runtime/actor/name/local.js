"use strict";
(function(){
  var Runtime = Dramatis.Runtime;
  var Name = Runtime.Actor.Name;
  var Local = Name.Local =
    new Dramatis.Runtime.Actor.Name.Class(function Local(args){
      switch(typeof args[0]) {
       case "string":
        this.id = args[0];
        break;
       case "number":
        this.id = args[0];
        break;
       case "object":
        if (args[0] instanceof String) {
          this.id = args[0].toString();
        } else {
          this._actor = args[0];
          this.id = this._actor.id;
        }
      }
    }, [ Runtime.Actor.Name.Base ], {
      uri: function() {
        var route = Dramatis.Director.current.route(this);
        return (new Name.Remote([route, this.id])).uri();
      },
      toJSON: function() {
        var route = Dramatis.Director.current.route(this);
        var json = (new Name.Remote([route, this.id])).toJSON();
        return json;
      },
      actor: function() {
        if (this._actor) {
          return this._actor;
        }
        var local = Dramatis.Director.current.lookup_actor(this.id);
        if (!local) {
          throw new Dramatis.Exception.
            NoSuchActor("No such actor: "+this.id);
        }
        if (typeof this.id === "number") {
          this._actor = local._actor;
        }
        return local._actor;
      }
    });
}());
