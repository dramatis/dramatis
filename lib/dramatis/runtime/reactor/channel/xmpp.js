(function(){
  var Channel = Dramatis.Runtime.Reactor.Channel;

  var parser = new RegExp("^bosh(s)?://([^:/]+)(:([^/]+))?(/(.+))?/([^@:/]+)(:([^@]+))(@([^:/@]+))?$");

  var parse = function parse( xmpp, url ) {
    var undefined;
    var match = parser.exec(url);
    if(match){
      if(match[1]){
        throw new Error("implement https");
      };
      xmpp.service = "http://"+match[2];
      if(match[4]){
        xmpp.service += ":"+match[4];
      }
      if(match[6]){
        xmpp.service += "/"+match[6];
      }
      xmpp.jid = match[7] + "@";
      if(match[11]){
        xmpp.jid += match[11];
      } else {
        throw new Error("implement default vhost");
        xmpp.jid += match[2];
      }
      xmpp.password = match[9];
    }
  };

  var strophe_callback = function strophe_callback(status, reason){
// console.debug("strophe:",status,reason);
    if(status == Strophe.Status.CONNECTED && this._good){
      this._good(this);
    } else if (status == Strophe.Status.CONNFAIL && this._bad) {
      this.connection.abort();
      this._bad(reason);
    }
  };

  var XMPP = Channel.XMPP =
    new Channel.Class(function XMPP(url, good, bad){
      parse(this,url);
      this._good = good;
      this._bad = bad;
      var self = this;
      var callback = function callback() {
        strophe_callback.apply( self, arguments );
      };
      this.connection = new Strophe.Connection(this.service);
      this.connection.connect(this.jid, this.password, callback);
    });
})();
