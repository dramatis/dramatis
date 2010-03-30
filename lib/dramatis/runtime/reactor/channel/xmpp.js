"use strict";
/*global Dramatis, Strophe, window*/
(function () {
  var Channel = Dramatis.Runtime.Reactor.Channel;

  var parser = new RegExp("^bosh(s)?://([^:/]+)(:([^/]+))?(/(.+))?/([^@:/]+)(:([^@]+))(@([^:/@]+))?(/(.*))?$");

  var parse = function parse( xmpp, url ) {
    var match = parser.exec(url);
    if(match){
      if(match[1]){
        throw new Error("implement https");
      }
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
        // xmpp.jid += match[2];
      }
      if(match[12]){
        xmpp.jid += match[12];
      }
      xmpp.password = match[9];
    }
  };

  var debug = window.console &&
              window.console.log &&
              function(){window.console.log.apply(window.console,arguments);} || function(){};
  var parent_regex = new RegExp("/[^/]*$");

  var new_iframe = function new_iframe(fn) {
    var self = this;
    var iframe = window.document.createElement('iframe');
    var parent = this.service;
    parent = parent.replace( parent_regex, "" );
    parent = parent + "/pub/archive/index.html";
    iframe.setAttribute('src',parent);
    iframe.setAttribute('style',"display: none");
    iframe.onload = function(){ fn.call(self,iframe); };
    window.document.body.appendChild(iframe);
    /* this works around an issue with env.js which doesn't load the frame,
     * but also doesn't do same domain checking */
    if(!iframe.contentWindow){
      window.setTimeout(function(){fn.call(self,iframe);},0);
    } else {
      window.setTimeout(function(){fn.call(self,iframe);},1000);
    }
  };

  var statuses = [];
  (function(){
    for(var key in Strophe.Status) {
      statuses[Strophe.Status[key]]=key;
    }
  }());

  var strophe_callback = function strophe_callback(status, reason){
    var global = (function(){return this;}());
    debug("SC",this.connected,statuses[status].toLowerCase(),global.$.print(reason));
    if(status === Strophe.Status.CONNECTED && this._good){
      this.connected = true;
      this.connecting = false;
      this.uri = function(){
        return this.connection.jid ? "xmpp:" + this.connection.jid : "";
      };
      this._good(this);
    } else if (status === Strophe.Status.DISCONNECTED && this.connected) {
      this._call_on_disconnects(reason && reason !== null && reason || this._disconnect_reason);
    } else if (status === Strophe.Status.CONNFAIL && this.connected) {
      this._disconnect_reason = reason;
    } else if (status === Strophe.Status.AUTHFAIL && this.connecting) {
      this.connecting = false;
      if (this._bad) { this._bad(reason); }
    } else if (status === Strophe.Status.CONNFAIL && this.connecting) {
      if (this.connection.abort) {
        this.connection.abort();
      }
      this.connection = new Strophe.Connection(this.service);
      if(window.location.href.indexOf("file://")!==0){
        window.document.domain = window.document.domain;
      }
      // debug(this.connecting, this.connecting_direct);
      if (false && this.connecting && this.connecting_direct) {
        new_iframe.call(this, function(iframe){
          // debug("nif");
          this.connecting_direct = false;
          var self = this;
          var xmpp_window = iframe.contentWindow;
          try {
            if (xmpp_window.document) {}
          } catch(e) {
            xmpp_window = window;
          }
          // debug(xmpp_window.document);
          var callback = function callback() {
            strophe_callback.apply( self, arguments );
          };
          var undef;
          this.connection.connect(this.jid, this.password, callback, undef, undef, xmpp_window);
        });
      } else {
        this.connecting = false;
        if (this._bad) {
            this._bad(reason);
        }
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
    }, [Channel] );

}());
