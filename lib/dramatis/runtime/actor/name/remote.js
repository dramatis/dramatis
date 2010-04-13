"use strict";
(function(){
  var Runtime = Dramatis.Runtime;
  var Remote = Runtime.Actor.Name.Remote =
    new Dramatis.Runtime.Actor.Name.Class(function Remote(args){
      var parsed;
      if (args[0] instanceof Runtime.Reactor.Channel.Route) {
        this.route = args[0];
        this.id = args[1];
      } else if ((parsed = Runtime.Reactor.parse_url(args[0]))) {
        if (!(parsed.route instanceof Dramatis.Runtime.Reactor.Channel.Route)) {
          throw new Error(parsed +" is not a Route type");
        }
        this.route = parsed.route;
        this.id = parsed.id;
      } else {
        throw new Error("cannot parse actor name url "+args[0]);
      }
    }, [ Runtime.Actor.Name.Base ], {
      uri: function() {
        return this.route.uri(this.id);
      },
      toJSON: function() {
        return {
          "__jsonclass__": [
            this.constructor+"",
            this.route+"",
            this.id
          ]};
      }
    });

  Remote.fromJSON = function(args) {
    var route = Runtime.Reactor.parse_url(args[0]).route;
    var id = args[1];
    return Dramatis.Director.current.lookup(route, id) || new Remote([route, id]);
  };

}());
