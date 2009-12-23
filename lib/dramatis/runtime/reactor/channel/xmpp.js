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

  var debug = window.console && window.console.debug || window.debug;
  var parent_regex = new RegExp("/[^/]*$");

  var new_iframe = function new_iframe(fn) {
    var self = this;
    var iframe = document.createElement('iframe');
    var parent = this.service;
    parent = parent.replace( parent_regex, "" );
    parent = parent + "/pub/archive/index.html";
    iframe.setAttribute('src',parent);
    iframe.setAttribute('style',"display: none");
    iframe.onload = function(){ fn.call(self,iframe); };
    document.body.appendChild(iframe);
    /* this works around an issue with env.js which doesn't load the frame,
     * but also doesn't do same domain checking */
    if(!iframe.contentWindow){
      setTimeout(function(){fn.call(self,iframe)},0);
    } else {
      setTimeout(function(){fn.call(self,iframe)},1000);
    }
  };

  var strophe_callback = function strophe_callback(status, reason){
    // debug("sc",status,reason);
    if(status == Strophe.Status.CONNECTED && this._good){
      this.connecting = false;
      this._good(this);
    } else if (status == Strophe.Status.CONNFAIL) {
      this.connection.abort();
      this.connection = new Strophe.Connection(this.service);
      if(window.location.href.indexOf("file://")!==0){
        document.domain = document.domain;
      }
      // debug(this.connecting, this.connecting_direct);
      if (this.connecting && this.connecting_direct) {
        new_iframe.call(this, function(iframe){
          // debug("nif");
          this.connecting_direct = false;
          var undefined;
          var self = this;
          var xmpp_window = iframe.contentWindow;
          try {
            xmpp_window.document;
          } catch(e) {
            xmpp_window = window;
          }
          // debug(xmpp_window.document);
          var callback = function callback() {
            strophe_callback.apply( self, arguments );
          };
          this.connection.connect(this.jid, this.password, callback, undefined, undefined, xmpp_window);
        });
      } else {
        this.connecting = false;
        this._bad && this._bad(reason);
      }
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
      this.connecting = this.connecting_direct = true;
      this.connection = new Strophe.Connection(this.service);
      this.connection.connect(this.jid, this.password, callback);
    });

})();
