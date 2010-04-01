"use strict";
(function(){
  var Runtime = Dramatis.Runtime;
  var Remote = Runtime.Actor.Name.Remote =
    new Dramatis.Runtime.Actor.Name.Class(function Remote(args){
      var string = args[0];
      var parsed;
      if ((parsed = Runtime.Reactor.parse_url(string))) {
        if (!(parsed.route instanceof Dramatis.Runtime.Reactor.Channel.Route)) {
          throw new Error(parsed +" is not a Route type");
        }
        this.route = parsed.route;
        this.actor = parsed.actor;
      } else {
        throw new Error("cannot parse actor name url "+string);
      }
    }, [ Runtime.Actor.Name.Base] );
}());
