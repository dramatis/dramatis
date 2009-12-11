(function(){
  var Reactor = Dramatis.Runtime.Reactor;
  var Channel = Reactor.Channel =
    new Dramatis.Runtime.Reactor.Class(function Channel(url,good,bad){
      if(url.match(/^bosh:\/\//i)){
        return new Channel.XMPP(url,good,bad);
      } else {
        throw new BadURL(url);
      }
    });
  new Dramatis.Class.Subscope(Channel);

  var BadURL = Channel.BadURL = function(url){
    Error.call(this,"BadURL: could not parse "+url);
  };
  BadURL.prototype = new Error;
  BadURL.prototype.constructor = BadURL;

  var NoConnection = Channel.NoConnection = function(url){
    Error.call(this,"NoConnectiohn: could not conect to "+url);
  };
  NoConnection.prototype = new Error;
  NoConnection.prototype.constructor = NoConnection;

})();
