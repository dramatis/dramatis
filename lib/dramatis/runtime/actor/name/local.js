"use strict";
(function(){
  var Runtime = Dramatis.Runtime;
  var Name = Runtime.Actor.Name;
  var Local = Name.Local =
    new Dramatis.Runtime.Actor.Name.Class(function Local(args){
      // console.debug($.print(args));
      this.actor = args[0];
      this.id = this.actor.id;
    }, [ Runtime.Actor.Name.Base ], {
      toJSON: function() {
        var route = Dramatis.Director.current.route(this);
        return (new Name.Remote([route, this.id])).toJSON();
      }
    });
}());
