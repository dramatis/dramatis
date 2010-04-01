"use strict";
/*global Dramatis*/
(function(){
  var Reactor = Dramatis.Runtime.Reactor;
  var Channel = Reactor.Channel =
    new Dramatis.Runtime.Reactor.Class(function Channel(url,good,bad){
      if(arguments.length) {
        if(url.match(/^bosh:\/\//i)){
          return new Channel.XMPP(url,good,bad);
        } else {
          throw new Channel.BadURL(url);
        }
      }
      return this; // keep emacs happy
    }, {
      on_disconnect: function(callback) {
        this._on_disconnect = this._on_disconnect || [];
        this._on_disconnect.push(callback);
      },
      _call_on_disconnects: function() {
        var disconnect;
        var global = (function(){return this;}());
        while (this._on_disconnect && (disconnect = this._on_disconnect.shift())) {
          disconnect.apply(global,arguments);
        }
      }
    });
  new Dramatis.Class.Subscope(Channel);

  var BadURL = Channel.BadURL = function(url){
    Error.call(this,"BadURL: could not parse "+url);
  };
  BadURL.prototype = new Error();
  BadURL.prototype.constructor = BadURL;

  var NoConnection = Channel.NoConnection = function(url){
    Error.call(this,"NoConnectiohn: could not conect to "+url);
  };
  NoConnection.prototype = new Error();
  NoConnection.prototype.constructor = NoConnection;

  Dramatis.Runtime.Reactor.Channel.Route =
    new Channel.Class(function Route(){
    });

}());
