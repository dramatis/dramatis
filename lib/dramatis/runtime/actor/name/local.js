"use strict";
(function(){
  var Runtime = Dramatis.Runtime;
  var Local = Runtime.Actor.Name.Local =
    new Dramatis.Runtime.Actor.Name.Class(function Local(args){
      this.actor = args[0];
    }, [ Runtime.Actor.Name.Base] );
}());
